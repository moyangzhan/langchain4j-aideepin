package com.moyz.adi.common.service;

import com.moyz.adi.common.helper.LLMContext;
import com.moyz.adi.common.interfaces.TriConsumer;
import com.moyz.adi.common.util.AdiPgVectorEmbeddingStore;
import com.moyz.adi.common.vo.AnswerMeta;
import com.moyz.adi.common.vo.PromptMeta;
import dev.langchain4j.data.document.Document;
import dev.langchain4j.data.document.DocumentSplitter;
import dev.langchain4j.data.document.splitter.DocumentSplitters;
import dev.langchain4j.data.embedding.Embedding;
import dev.langchain4j.data.message.AiMessage;
import dev.langchain4j.data.segment.TextSegment;
import dev.langchain4j.model.embedding.AllMiniLmL6V2EmbeddingModel;
import dev.langchain4j.model.embedding.EmbeddingModel;
import dev.langchain4j.model.input.Prompt;
import dev.langchain4j.model.input.PromptTemplate;
import dev.langchain4j.model.openai.OpenAiTokenizer;
import dev.langchain4j.model.output.Response;
import dev.langchain4j.store.embedding.EmbeddingMatch;
import dev.langchain4j.store.embedding.EmbeddingStore;
import dev.langchain4j.store.embedding.EmbeddingStoreIngestor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.commons.lang3.tuple.Triple;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static dev.langchain4j.model.openai.OpenAiModelName.GPT_3_5_TURBO;
import static java.util.stream.Collectors.joining;

@Slf4j
@Service
public class RAGService {
    @Value("${spring.datasource.url}")
    private String dataBaseUrl;

    @Value("${spring.datasource.username}")
    private String dataBaseUserName;

    @Value("${spring.datasource.password}")
    private String dataBasePassword;
    private static final PromptTemplate promptTemplate = PromptTemplate.from("尽可能准确地回答下面的问题: {{question}}\n\n根据以下知识库的内容:\n{{information}}");

    private EmbeddingModel embeddingModel;

    private EmbeddingStore<TextSegment> embeddingStore;

    public void init() {
        log.info("initEmbeddingModel");
        embeddingModel = new AllMiniLmL6V2EmbeddingModel();
        embeddingStore = initEmbeddingStore();
    }

    private EmbeddingStore<TextSegment> initEmbeddingStore() {
        // 正则表达式匹配
        String regex = "jdbc:postgresql://([^:/]+):(\\d+)/(\\w+).+";
        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(dataBaseUrl);

        String host = "";
        String port = "";
        String databaseName = "";
        if (matcher.matches()) {
            host = matcher.group(1);
            port = matcher.group(2);
            databaseName = matcher.group(3);

            System.out.println("Host: " + host);
            System.out.println("Port: " + port);
            System.out.println("Database: " + databaseName);
        } else {
            throw new RuntimeException("parse url error");
        }
        AdiPgVectorEmbeddingStore embeddingStore = AdiPgVectorEmbeddingStore.builder()
                .host(host)
                .port(Integer.parseInt(port))
                .database(databaseName)
                .user(dataBaseUserName)
                .password(dataBasePassword)
                .dimension(384)
                .createTable(true)
                .dropTableFirst(false)
                .table("adi_knowledge_base_embedding")
                .build();
        return embeddingStore;
    }

    private EmbeddingStoreIngestor getEmbeddingStoreIngestor() {
        DocumentSplitter documentSplitter = DocumentSplitters.recursive(1000, 0, new OpenAiTokenizer(GPT_3_5_TURBO));
        EmbeddingStoreIngestor embeddingStoreIngestor = EmbeddingStoreIngestor.builder()
                .documentSplitter(documentSplitter)
                .embeddingModel(embeddingModel)
                .embeddingStore(embeddingStore)
                .build();
        return embeddingStoreIngestor;
    }

    /**
     * 对文档切块并向量化
     *
     * @param document 知识库文档
     */
    public void ingest(Document document) {
        getEmbeddingStoreIngestor().ingest(document);
    }

    public Prompt retrieveAndCreatePrompt(String kbUuid, String question) {
        // Embed the question
        Embedding questionEmbedding = embeddingModel.embed(question).content();

        // Find relevant embeddings in embedding store by semantic similarity
        // You can play with parameters below to find a sweet spot for your specific use case
        int maxResults = 3;
        double minScore = 0.6;
        List<EmbeddingMatch<TextSegment>> relevantEmbeddings = ((AdiPgVectorEmbeddingStore) embeddingStore).findRelevantByKbUuid(kbUuid, questionEmbedding, maxResults, minScore);

        // Create a prompt for the model that includes question and relevant embeddings
        String information = relevantEmbeddings.stream()
                .map(match -> match.embedded().text())
                .collect(joining("\n\n"));

        if (StringUtils.isBlank(information)) {
            return null;
        }
        return promptTemplate.apply(Map.of("question", question, "information", Matcher.quoteReplacement(information)));
    }

    /**
     * 召回并提问
     *
     * @param kbUuid    知识库uuid
     * @param question  用户的问题
     * @param modelName LLM model name
     * @return
     */
    public Pair<String, Response<AiMessage>> retrieveAndAsk(String kbUuid, String question, String modelName) {
        Prompt prompt = retrieveAndCreatePrompt(kbUuid, question);
        if (null == prompt) {
            return null;
        }
        Response<AiMessage> response = new LLMContext(modelName).getLLMService().chat(prompt.toUserMessage());
        return new ImmutablePair<>(prompt.text(), response);
    }
}

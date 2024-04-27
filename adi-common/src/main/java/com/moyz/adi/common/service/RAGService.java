package com.moyz.adi.common.service;

import com.moyz.adi.common.helper.LLMContext;
import com.moyz.adi.common.util.AdiPgVectorEmbeddingStore;
import dev.langchain4j.data.document.Document;
import dev.langchain4j.data.document.DocumentSplitter;
import dev.langchain4j.data.document.splitter.DocumentSplitters;
import dev.langchain4j.data.embedding.Embedding;
import dev.langchain4j.data.message.AiMessage;
import dev.langchain4j.data.segment.TextSegment;
import dev.langchain4j.model.embedding.AllMiniLmL6V2EmbeddingModel;
import dev.langchain4j.model.embedding.EmbeddingModel;
import dev.langchain4j.model.input.Prompt;
import dev.langchain4j.model.openai.OpenAiTokenizer;
import dev.langchain4j.model.output.Response;
import dev.langchain4j.rag.content.retriever.ContentRetriever;
import dev.langchain4j.rag.content.retriever.EmbeddingStoreContentRetriever;
import dev.langchain4j.store.embedding.EmbeddingMatch;
import dev.langchain4j.store.embedding.EmbeddingStore;
import dev.langchain4j.store.embedding.EmbeddingStoreIngestor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;

import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static com.moyz.adi.common.cosntant.AdiConstant.PROMPT_TEMPLATE;
import static dev.langchain4j.model.openai.OpenAiModelName.GPT_3_5_TURBO;
import static java.util.stream.Collectors.joining;

@Slf4j
public class RAGService {

    private static final int MAX_RESULTS = 3;
    private static final double MIN_SCORE = 0.6;

    private String dataBaseUrl;

    private String dataBaseUserName;

    private String dataBasePassword;

    private String tableName;

    private EmbeddingModel embeddingModel;

    private EmbeddingStore<TextSegment> embeddingStore;

    public RAGService(String tableName, String dataBaseUrl, String dataBaseUserName, String dataBasePassword) {
        this.tableName = tableName;
        this.dataBasePassword = dataBasePassword;
        this.dataBaseUserName = dataBaseUserName;
        this.dataBaseUrl = dataBaseUrl;
    }

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
                .table(tableName)
                .build();
        return embeddingStore;
    }

    /**
     * 对文档切块、向量化并存储到数据库
     *
     * @param document 知识库文档
     */
    public void ingest(Document document) {
        DocumentSplitter documentSplitter = DocumentSplitters.recursive(1000, 0, new OpenAiTokenizer(GPT_3_5_TURBO));
        EmbeddingStoreIngestor embeddingStoreIngestor = EmbeddingStoreIngestor.builder()
                .documentSplitter(documentSplitter)
                .embeddingModel(embeddingModel)
                .embeddingStore(embeddingStore)
                .build();
        embeddingStoreIngestor.ingest(document);
    }

    /**
     * There are two methods for retrieve documents:
     * 1. ContentRetriever.retrieve()
     * 2. retrieveAndCreatePrompt()
     *
     * @return ContentRetriever
     */
    public ContentRetriever buildContentRetriever() {
        return EmbeddingStoreContentRetriever.builder()
                .embeddingStore(embeddingStore)
                .embeddingModel(embeddingModel)
                .maxResults(MAX_RESULTS)
                .minScore(MIN_SCORE)
                .build();
    }

    /**
     * Retrieve documents and create prompt
     *
     * @param metadataCond Query condition
     * @param question     User's question
     * @return Document in the vector db
     */
    public Prompt retrieveAndCreatePrompt(Map<String, String> metadataCond, String question) {
        // Embed the question
        Embedding questionEmbedding = embeddingModel.embed(question).content();

        // Find relevant embeddings in embedding store by semantic similarity
        // You can play with parameters below to find a sweet spot for your specific use case
        int maxResults = 3;
        double minScore = 0.6;
        List<EmbeddingMatch<TextSegment>> relevantEmbeddings = ((AdiPgVectorEmbeddingStore) embeddingStore).findRelevantByMetadata(metadataCond, questionEmbedding, maxResults, minScore);

        // Create a prompt that includes question and relevant embeddings
        String information = relevantEmbeddings.stream()
                .map(match -> match.embedded().text())
                .collect(joining("\n\n"));

        if (StringUtils.isBlank(information)) {
            return null;
        }
        return PROMPT_TEMPLATE.apply(Map.of("question", question, "information", Matcher.quoteReplacement(information)));
    }

    /**
     * 召回并提问
     *
     * @param metadataCond metadata condition
     * @param question     user's question
     * @param modelName    LLM model name
     * @return
     */
    public Pair<String, Response<AiMessage>> retrieveAndAsk(Map<String, String> metadataCond, String question, String modelName) {

        Prompt prompt = retrieveAndCreatePrompt(metadataCond, question);
        if (null == prompt) {
            return null;
        }
        Response<AiMessage> response = new LLMContext(modelName).getLLMService().chat(prompt.toUserMessage());
        return new ImmutablePair<>(prompt.text(), response);
    }

    public static final String parsePromptTemplate(String question, String information) {
        return PROMPT_TEMPLATE.apply(Map.of("question", question, "information", Matcher.quoteReplacement(information))).text();
    }
}

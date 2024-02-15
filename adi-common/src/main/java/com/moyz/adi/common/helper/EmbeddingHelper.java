package com.moyz.adi.common.helper;

import com.moyz.adi.common.util.AdiPgVectorEmbeddingStore;
import dev.langchain4j.data.document.DocumentSplitter;
import dev.langchain4j.data.document.splitter.DocumentSplitters;
import dev.langchain4j.data.embedding.Embedding;
import dev.langchain4j.data.message.AiMessage;
import dev.langchain4j.data.segment.TextSegment;
import dev.langchain4j.model.chat.ChatLanguageModel;
import dev.langchain4j.model.embedding.AllMiniLmL6V2EmbeddingModel;
import dev.langchain4j.model.embedding.EmbeddingModel;
import dev.langchain4j.model.input.Prompt;
import dev.langchain4j.model.input.PromptTemplate;
import dev.langchain4j.model.openai.OpenAiChatModel;
import dev.langchain4j.model.openai.OpenAiTokenizer;
import dev.langchain4j.store.embedding.EmbeddingMatch;
import dev.langchain4j.store.embedding.EmbeddingStore;
import dev.langchain4j.store.embedding.EmbeddingStoreIngestor;
import jakarta.annotation.PostConstruct;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.net.InetSocketAddress;
import java.net.Proxy;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static dev.langchain4j.model.openai.OpenAiModelName.GPT_3_5_TURBO;
import static java.util.stream.Collectors.joining;

@Slf4j
@Service
public class EmbeddingHelper {

    @Value("${spring.datasource.url}")
    private String dataBaseUrl;

    @Value("${spring.datasource.username}")
    private String dataBaseUserName;

    @Value("${spring.datasource.password}")
    private String dataBasePassword;

    @Value("${openai.proxy.enable:false}")
    private boolean proxyEnable;

    @Value("${openai.proxy.host:0}")
    private String proxyHost;

    @Value("${openai.proxy.http-port:0}")
    private int proxyHttpPort;

    private static final PromptTemplate promptTemplate = PromptTemplate.from("尽可能准确地回答下面的问题: {{question}}\n\n根据以下知识库的内容:\n{{information}}");

    @Resource
    private OpenAiHelper openAiHelper;

    private EmbeddingModel embeddingModel;

    private EmbeddingStore<TextSegment> embeddingStore;

    private ChatLanguageModel chatLanguageModel;

    public void init() {
        log.info("initEmbeddingModel");
        embeddingModel = new AllMiniLmL6V2EmbeddingModel();
        embeddingStore = initEmbeddingStore();
        chatLanguageModel = initChatLanguageModel();
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

    private ChatLanguageModel initChatLanguageModel() {
        OpenAiChatModel.OpenAiChatModelBuilder builder = OpenAiChatModel.builder().apiKey(openAiHelper.getSecretKey());
        if (proxyEnable) {
            Proxy proxy = new Proxy(Proxy.Type.HTTP, new InetSocketAddress(proxyHost, proxyHttpPort));
            builder.proxy(proxy);
        }
        return builder.build();
    }

    public EmbeddingStoreIngestor getEmbeddingStoreIngestor() {
        DocumentSplitter documentSplitter = DocumentSplitters.recursive(1000, 0, new OpenAiTokenizer(GPT_3_5_TURBO));
        EmbeddingStoreIngestor embeddingStoreIngestor = EmbeddingStoreIngestor.builder()
                .documentSplitter(documentSplitter)
                .embeddingModel(embeddingModel)
                .embeddingStore(embeddingStore)
                .build();
        return embeddingStoreIngestor;
    }

    public String findAnswer(String kbUuid, String question) {

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
            return StringUtils.EMPTY;
        }
        Prompt prompt = promptTemplate.apply(Map.of("question", question, "information", Matcher.quoteReplacement(information)));

        AiMessage aiMessage = chatLanguageModel.generate(prompt.toUserMessage()).content();

        // See an answer from the model
        return aiMessage.text();
    }

}

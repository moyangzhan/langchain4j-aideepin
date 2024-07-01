package com.moyz.adi.common.service;

import dev.langchain4j.data.document.Document;
import dev.langchain4j.data.document.DocumentSplitter;
import dev.langchain4j.data.document.splitter.DocumentSplitters;
import dev.langchain4j.data.segment.TextSegment;
import dev.langchain4j.model.embedding.AllMiniLmL6V2EmbeddingModel;
import dev.langchain4j.model.embedding.EmbeddingModel;
import dev.langchain4j.model.openai.OpenAiTokenizer;
import dev.langchain4j.rag.content.retriever.ContentRetriever;
import dev.langchain4j.rag.content.retriever.EmbeddingStoreContentRetriever;
import dev.langchain4j.store.embedding.EmbeddingStore;
import dev.langchain4j.store.embedding.EmbeddingStoreIngestor;
import dev.langchain4j.store.embedding.filter.Filter;
import dev.langchain4j.store.embedding.filter.comparison.IsEqualTo;
import dev.langchain4j.store.embedding.pgvector.PgVectorEmbeddingStore;
import lombok.extern.slf4j.Slf4j;

import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static com.moyz.adi.common.cosntant.AdiConstant.*;
import static dev.langchain4j.model.openai.OpenAiModelName.GPT_3_5_TURBO;

@Slf4j
public class RAGService {

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
        PgVectorEmbeddingStore embeddingStore = PgVectorEmbeddingStore.builder()
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
        DocumentSplitter documentSplitter = DocumentSplitters.recursive(RAG_MAX_SEGMENT_SIZE_IN_TOKENS, 0, new OpenAiTokenizer(GPT_3_5_TURBO));
        EmbeddingStoreIngestor embeddingStoreIngestor = EmbeddingStoreIngestor.builder()
                .documentSplitter(documentSplitter)
                .embeddingModel(embeddingModel)
                .embeddingStore(embeddingStore)
                .build();
        embeddingStoreIngestor.ingest(document);
    }

    public ContentRetriever createRetriever(Map<String, String> metadataCond, int maxResults) {
        Filter filter = null;
        for (String key : metadataCond.keySet()) {
            if (null == filter) {
                filter = new IsEqualTo(key, metadataCond.get(key));
            } else {
                filter = filter.and(new IsEqualTo(key, metadataCond.get(key)));
            }
        }
        return EmbeddingStoreContentRetriever.builder()
                .embeddingStore(embeddingStore)
                .embeddingModel(embeddingModel)
                .maxResults(maxResults)
                .minScore(RAG_MIN_SCORE)
                .filter(filter)
                .build();
    }

    public static final String parsePromptTemplate(String question, String information) {
        return PROMPT_TEMPLATE.apply(Map.of("question", question, "information", Matcher.quoteReplacement(information))).text();
    }

    /**
     * 根据模型的contentWindow计算使用该模型最多召回的文档数量
     *
     * @param userQuestion  用户问题
     * @param contentWindow AI模型所能容纳的窗口大小
     * @return
     */
    public static int getRetrieveMaxResults(String userQuestion, int contentWindow) {
        int questionLength = new OpenAiTokenizer(GPT_3_5_TURBO).estimateTokenCountInText(userQuestion);
        int maxRetrieveDocLength = contentWindow - questionLength;
        if (maxRetrieveDocLength < RAG_MAX_SEGMENT_SIZE_IN_TOKENS) {
            return RAG_MAX_RESULTS_DEFAULT;
        } else if (maxRetrieveDocLength > RAG_NUMBER_RETURN_MAX * RAG_MAX_SEGMENT_SIZE_IN_TOKENS) {
            return RAG_NUMBER_RETURN_MAX;
        } else {
            return (int) Math.ceil(maxRetrieveDocLength / RAG_MAX_SEGMENT_SIZE_IN_TOKENS);
        }
    }
}

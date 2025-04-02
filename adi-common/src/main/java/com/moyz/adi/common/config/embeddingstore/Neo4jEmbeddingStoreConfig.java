package com.moyz.adi.common.config.embeddingstore;

import com.moyz.adi.common.rag.neo4j.AdiNeo4jEmbeddingStore;
import dev.langchain4j.data.segment.TextSegment;
import dev.langchain4j.store.embedding.EmbeddingStore;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;

@Slf4j
@Configuration
@ConditionalOnProperty(value = "adi.vector-database", havingValue = "neo4j")
public class Neo4jEmbeddingStoreConfig {

    @Value("${adi.datasource.neo4j.host}")
    private String dbHost;

    @Value("${adi.datasource.neo4j.port}")
    private String dbPort;

    @Value("${adi.datasource.neo4j.username}")
    private String dbUserName;

    @Value("${adi.datasource.neo4j.password}")
    private String dbPassword;

    @Value("${adi.datasource.neo4j.database}")
    private String database;

    @Bean(name = "kbEmbeddingStore")
    @Primary
    public EmbeddingStore<TextSegment> initKbEmbeddingStore() {
        return AdiNeo4jEmbeddingStore.builder()
                .databaseName(database)
                .indexName("embedding")
                .withBasicAuth("neo4j://" + dbHost + ":" + dbPort, dbUserName, dbPassword)
                .dimension(384)
                .label("adi_knowledge_base_embedding")
                .build();
    }

    @Bean(name = "searchEmbeddingStore")
    public EmbeddingStore<TextSegment> initSearchEmbeddingStore() {
        return AdiNeo4jEmbeddingStore.builder()
                .databaseName(database)
                .indexName("aisearch")
                .withBasicAuth("neo4j://" + dbHost + ":" + dbPort, dbUserName, dbPassword)
                .dimension(384)
                .label("adi_ai_search_embedding")
                .build();
    }
}

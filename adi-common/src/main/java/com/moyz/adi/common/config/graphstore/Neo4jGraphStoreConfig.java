package com.moyz.adi.common.config.graphstore;

import com.moyz.adi.common.rag.GraphStore;
import com.moyz.adi.common.rag.Neo4jGraphStore;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;

@Slf4j
@Configuration
@ConditionalOnProperty(value = "adi.graph-database", havingValue = "neo4j")
public class Neo4jGraphStoreConfig {

    @Value("${adi.datasource.neo4j.host}")
    private String dbHost;

    @Value("${adi.datasource.neo4j.port}")
    private Integer dbPort;

    @Value("${adi.datasource.neo4j.username}")
    private String dbUserName;

    @Value("${adi.datasource.neo4j.password}")
    private String dbPassword;

    @Bean(name = "kbGraphStore")
    @Primary
    public GraphStore initGraphStore() {
        return Neo4jGraphStore
                .builder()
                .host(dbHost)
                .port(dbPort)
                .user(dbUserName)
                .password(dbPassword)
                .graphName("adi_knowledge_base_graph")
                .dropGraphFirst(false)
                .build();
    }
}

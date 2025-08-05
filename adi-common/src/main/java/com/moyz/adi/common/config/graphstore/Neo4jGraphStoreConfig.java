package com.moyz.adi.common.config.graphstore;

import com.moyz.adi.common.config.AdiProperties;
import com.moyz.adi.common.rag.GraphStore;
import com.moyz.adi.common.rag.neo4j.Neo4jGraphStore;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;

@Slf4j
@Configuration
@ConditionalOnProperty(value = "adi.graph-database", havingValue = "neo4j")
public class Neo4jGraphStoreConfig {

    @Resource
    private AdiProperties adiProperties;

    @Bean(name = "kbGraphStore")
    @Primary
    public GraphStore initGraphStore() {
        AdiProperties.Neo4j neo4j = adiProperties.getDatasource().getNeo4j();
        return Neo4jGraphStore
                .builder()
                .host(neo4j.getHost())
                .port(neo4j.getPort())
                .user(neo4j.getUsername())
                .password(neo4j.getPassword())
                .graphName("adi_knowledge_base_graph")
                .dropGraphFirst(false)
                .build();
    }
}

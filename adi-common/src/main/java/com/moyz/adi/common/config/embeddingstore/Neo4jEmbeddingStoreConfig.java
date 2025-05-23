package com.moyz.adi.common.config.embeddingstore;

import com.moyz.adi.common.config.AdiProperties;
import com.moyz.adi.common.rag.neo4j.AdiNeo4jEmbeddingStore;
import com.moyz.adi.common.util.AdiPropertiesUtil;
import dev.langchain4j.community.store.embedding.neo4j.Neo4jEmbeddingStore;
import dev.langchain4j.data.segment.TextSegment;
import dev.langchain4j.store.embedding.EmbeddingStore;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.DependsOn;
import org.springframework.context.annotation.Primary;


@Slf4j
@Configuration
@ConditionalOnProperty(value = "adi.vector-database", havingValue = "neo4j")
public class Neo4jEmbeddingStoreConfig {

    @Resource
    private AdiProperties adiProperties;

    /**
     * 根据选定的嵌入模型及其生成向量的维度不同来定义不同的表名，项目启动时只使用其中一种<br/>
     * 例如：<br/>
     * adi_knowledge_base_embedding（默认）: 存储的数据是本地嵌入模型all-minilm-l6-v2生成的维度为384的向量<br/>
     * adi_knowledge_base_embedding_qwen_1024: 存储的数据是通义千问的嵌入模型生成的维度为1024的向量<br/>
     * adi_knowledge_base_embedding_openai_1536: 存储的数据是openai的嵌入模型生成的维度为1536的向量<br/>
     *
     * @return embeddingStore
     */
    @Bean(name = "kbEmbeddingStore")
    @Primary
    @DependsOn("initializer")
    public EmbeddingStore<TextSegment> initKbEmbeddingStore() {
        log.info("Initializing kbEmbeddingStore...");
        String tableName = "adi_knowledge_base_embedding";
        String indexName = "embedding";
        Pair<String, Integer> pair = AdiPropertiesUtil.getSuffixAndDimension(adiProperties);
        if (StringUtils.isNotBlank(pair.getLeft())) {
            tableName = tableName + "_" + pair.getLeft();
            indexName = indexName + "_" + pair.getLeft();
        }
        return createEmbeddingStore(indexName, tableName, pair.getRight());
    }

    @Bean(name = "searchEmbeddingStore")
    @DependsOn("initializer")
    public EmbeddingStore<TextSegment> initSearchEmbeddingStore() {
        log.info("Initializing searchEmbeddingStore...");
        String tableName = "adi_ai_search_embedding";
        String indexName = "aisearch";
        Pair<String, Integer> pair = AdiPropertiesUtil.getSuffixAndDimension(adiProperties);
        if (StringUtils.isNotBlank(pair.getLeft())) {
            tableName = tableName + "_" + pair.getLeft();
            indexName = indexName + "_" + pair.getLeft();
        }
        return createEmbeddingStore(indexName, tableName, pair.getRight());
    }

    private EmbeddingStore<TextSegment> createEmbeddingStore(String indexName, String tableName, int dimension) {
        log.info("Creating Neo4jEmbeddingStore with table name:{},dimension:{}", tableName, dimension);
        AdiProperties.Neo4j neo4j = adiProperties.getDatasource().getNeo4j();
        Neo4jEmbeddingStore neo4jEmbeddingStore = Neo4jEmbeddingStore.builder()
                .databaseName(neo4j.getDatabase())
                .indexName(indexName)
                .withBasicAuth("neo4j://" + neo4j.getHost() + ":" + neo4j.getPort(), neo4j.getUsername(), neo4j.getPassword())
                .dimension(dimension)
                .label(tableName)
//                .metadataPrefix("meta_")
                .build();
        return new AdiNeo4jEmbeddingStore(neo4jEmbeddingStore);
    }
}

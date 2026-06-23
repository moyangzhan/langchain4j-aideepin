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
     * adi_knowledge_base_embedding_bge_384（默认）: 本地嵌入模型bge-small-zh-v1.5，维度384<br/>
     * adi_knowledge_base_embedding: 本地嵌入模型all-minilm-l6-v2，维度384<br/>
     * adi_knowledge_base_embedding_qwen_1024: 通义千问嵌入模型，维度1024<br/>
     * adi_knowledge_base_embedding_openai_1536: OpenAI嵌入模型，维度1536<br/>
     * <p>
     * Define different table names based on the selected embedding model and its vector dimension.
     * Only one is used at runtime.<br/>
     * Examples:<br/>
     * adi_knowledge_base_embedding_bge_384: bge-small-zh-v1.5, dimension 384 (default)<br/>
     * adi_knowledge_base_embedding: all-minilm-l6-v2, dimension 384<br/>
     * adi_knowledge_base_embedding_qwen_1024: Qwen embedding model, dimension 1024<br/>
     * adi_knowledge_base_embedding_openai_1536: OpenAI embedding model, dimension 1536<br/>
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

    /**
     * 角色(会话)的长期记忆使用的向量库
     *
     * @return EmbeddingStore实例
     */
    @Bean(name = "characterMemoryEmbeddingStore")
    @DependsOn("initializer")
    public EmbeddingStore<TextSegment> initCharacterMemoryEmbeddingStore() {
        log.info("Initializing characterMemoryEmbeddingStore...");
        // Neo4j label/index names intentionally kept as "conversation" to avoid complex label rename + vector index rebuild
        String tableName = "adi_conversation_memory_embedding";
        String indexName = "conv_memory";
        Pair<String, Integer> pair = AdiPropertiesUtil.getSuffixAndDimension(adiProperties);
        if (StringUtils.isNotBlank(pair.getLeft())) {
            tableName = tableName + "_" + pair.getLeft();
            indexName = indexName + "_" + pair.getLeft();
        }
        return createEmbeddingStore(indexName, tableName, pair.getRight());
    }

    /**
     * 角色情景记忆使用的独立向量库。
     * <p>
     * Dedicated vector store for episodic memory, physically isolated from semantic
     * memory so retrieval on either store never competes for top-K with the other.
     *
     * @return EmbeddingStore实例
     */
    @Bean(name = "episodicMemoryEmbeddingStore")
    @DependsOn("initializer")
    public EmbeddingStore<TextSegment> initEpisodicMemoryEmbeddingStore() {
        log.info("Initializing episodicMemoryEmbeddingStore...");
        String tableName = "adi_character_episodic_memory_embedding";
        String indexName = "char_episodic_memory";
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
                .awaitIndexTimeout(120)
                .label(tableName)
//                .metadataPrefix("meta_")
                .build();
        return new AdiNeo4jEmbeddingStore(neo4jEmbeddingStore);
    }
}

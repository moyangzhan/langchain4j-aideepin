package com.moyz.adi.common.config.embeddingstore;

import com.moyz.adi.common.config.AdiProperties;
import com.moyz.adi.common.util.AdiPropertiesUtil;
import dev.langchain4j.data.segment.TextSegment;
import dev.langchain4j.store.embedding.EmbeddingStore;
import dev.langchain4j.store.embedding.pgvector.PgVectorEmbeddingStore;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.DependsOn;
import org.springframework.context.annotation.Primary;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * pgvector的相关配置
 */
@Slf4j
@Configuration
@ConditionalOnProperty(value = "adi.vector-database", havingValue = "pgvector")
public class PgVectorEmbeddingStoreConfig {

    @Value("${spring.datasource.url}")
    private String dataBaseUrl;

    @Value("${spring.datasource.username}")
    private String dataBaseUserName;

    @Value("${spring.datasource.password}")
    private String dataBasePassword;

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
    public EmbeddingStore<TextSegment> initKbEmbeddingStore() {
        log.info("Initializing kbEmbeddingStore...");
        String tableName = "adi_knowledge_base_embedding";
        Pair<String, Integer> pair = AdiPropertiesUtil.getSuffixAndDimension(adiProperties);
        if (StringUtils.isNotBlank(pair.getLeft())) {
            tableName = tableName + "_" + pair.getLeft();
        }
        return createEmbeddingStore(tableName, pair.getRight());
    }

    /**
     * 角色记忆使用的向量库
     *
     * @return EmbeddingStore实例
     */
    @Bean(name = "characterMemoryEmbeddingStore")
    public EmbeddingStore<TextSegment> initCharacterMemoryEmbeddingStore() {
        log.info("Initializing characterMemoryEmbeddingStore...");
        String tableName = "adi_character_memory_embedding";
        Pair<String, Integer> pair = AdiPropertiesUtil.getSuffixAndDimension(adiProperties);
        if (StringUtils.isNotBlank(pair.getLeft())) {
            tableName = tableName + "_" + pair.getLeft();
        }
        return createEmbeddingStore(tableName, pair.getRight());
    }

    private EmbeddingStore<TextSegment> createEmbeddingStore(String tableName, int dimension) {
        // 正则表达式匹配
        String regex = "jdbc:postgresql://([^:/]+):(\\d+)/(\\w+).*";
        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(dataBaseUrl);

        String host = "";
        String port = "";
        String databaseName = "";
        if (matcher.matches()) {
            host = matcher.group(1);
            port = matcher.group(2);
            databaseName = matcher.group(3);

            log.info("Host: " + host);
            log.info("Port: " + port);
            log.info("Database: " + databaseName);
        } else {
            throw new RuntimeException("parse url error");
        }
        log.info("Creating PgVectorEmbeddingStore with table name:{},dimension:{}", tableName, dimension);
        return PgVectorEmbeddingStore.builder()
                .host(host)
                .port(Integer.parseInt(port))
                .database(databaseName)
                .user(dataBaseUserName)
                .password(dataBasePassword)
                .dimension(dimension)
                .createTable(true)
                .dropTableFirst(false)
                .table(tableName)
                .build();
    }

}

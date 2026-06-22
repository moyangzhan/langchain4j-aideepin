package com.moyz.adi.common.config;

import com.moyz.adi.common.cosntant.AdiConstant;
import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

@Configuration
@ConfigurationProperties("adi")
@Data
public class AdiProperties {

    private String host;

    private String frontendUrl;

    private String backendUrl;

    private Proxy proxy;

    private String embeddingModel;

    private String vectorDatabase;

    private String graphDatabase;

    private Datasource datasource;

    private Encrypt encrypt;

    /**
     * 长期记忆相关配置 | Long-term memory settings.
     */
    private Memory memory = new Memory();

    @Data
    public static class Proxy {
        private boolean enable;
        private String host;
        private int httpPort;
    }

    @Data
    public static class Datasource {
        private Neo4j neo4j;
    }

    @Data
    public static class Neo4j {
        private String host;
        private int port;
        private String username;
        private String password;
        private String database;
    }

    @Data
    public static class Encrypt {
        private String aesKey;
    }

    /**
     * 长期记忆配置项。
     * <p>
     * Long-term memory configuration.
     */
    @Data
    public static class Memory {
        /**
         * 向量检索旧记忆时的最低相似度阈值。默认 {@link AdiConstant#LONG_TERM_MEMORY_MIN_SCORE_DEFAULT}。
         * <p>
         * Minimum similarity score for retrieving existing memories during extraction.
         */
        private double minScore = AdiConstant.LONG_TERM_MEMORY_MIN_SCORE_DEFAULT;
    }
}

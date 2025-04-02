package com.moyz.adi.common.rag.neo4j;

import org.neo4j.driver.AuthTokens;
import org.neo4j.driver.Driver;
import org.neo4j.driver.GraphDatabase;
import org.neo4j.driver.SessionConfig;

public class Builder {
    private String indexName;
    private String metadataPrefix;
    private String embeddingProperty;
    private String idProperty;
    private String label;
    private String textProperty;
    private String databaseName;
    private String retrievalQuery;
    private SessionConfig config;
    private Driver driver;
    private int dimension;
    private long awaitIndexTimeout;

    /**
     * @param indexName the optional index name (default: "vector")
     */
    public Builder indexName(String indexName) {
        this.indexName = indexName;
        return this;
    }

    /**
     * @param metadataPrefix the optional metadata prefix (default: "")
     */
    public Builder metadataPrefix(String metadataPrefix) {
        this.metadataPrefix = metadataPrefix;
        return this;
    }

    /**
     * @param embeddingProperty the optional embeddingProperty name (default: "embedding")
     */
    public Builder embeddingProperty(String embeddingProperty) {
        this.embeddingProperty = embeddingProperty;
        return this;
    }

    /**
     * @param idProperty the optional id property name (default: "id")
     */
    public Builder idProperty(String idProperty) {
        this.idProperty = idProperty;
        return this;
    }

    /**
     * @param label the optional label name (default: "Document")
     */
    public Builder label(String label) {
        this.label = label;
        return this;
    }

    /**
     * @param textProperty the optional textProperty property name (default: "text")
     */
    public Builder textProperty(String textProperty) {
        this.textProperty = textProperty;
        return this;
    }

    /**
     * @param databaseName the optional database name (default: "neo4j")
     */
    public Builder databaseName(String databaseName) {
        this.databaseName = databaseName;
        return this;
    }

    /**
     * @param retrievalQuery the optional retrieval query
     *                       (default: "RETURN properties(node) AS metadata, node.`idProperty` AS `idProperty`, node.`textProperty` AS `textProperty`, node.`embeddingProperty` AS `embeddingProperty`, score")
     */
    public Builder retrievalQuery(String retrievalQuery) {
        this.retrievalQuery = retrievalQuery;
        return this;
    }

    /**
     * @param config the {@link SessionConfig}  (optional, default is `SessionConfig.forDatabase(`databaseName`)`)
     */
    public Builder config(SessionConfig config) {
        this.config = config;
        return this;
    }

    /**
     * @param driver the {@link Driver} (required)
     */
    public Builder driver(Driver driver) {
        this.driver = driver;
        return this;
    }

    /**
     * @param dimension the dimension (required)
     */
    public Builder dimension(int dimension) {
        this.dimension = dimension;
        return this;
    }

    /**
     * @param awaitIndexTimeout the optional awaiting timeout for all indexes to come online, in seconds (default: 60s)
     */
    public Builder awaitIndexTimeout(long awaitIndexTimeout) {
        this.awaitIndexTimeout = awaitIndexTimeout;
        return this;
    }

    /**
     * Creates an instance a {@link Driver}, starting from uri, user and password
     *
     * @param uri      the Bolt URI to a Neo4j instance
     * @param user     the Neo4j instance's username
     * @param password the Neo4j instance's password
     */
    public Builder withBasicAuth(String uri, String user, String password) {
        this.driver = GraphDatabase.driver(uri, AuthTokens.basic(user, password));
        return this;
    }

    public AdiNeo4jEmbeddingStore build() {
        return new AdiNeo4jEmbeddingStore(
                config,
                driver,
                dimension,
                label,
                embeddingProperty,
                idProperty,
                metadataPrefix,
                textProperty,
                indexName,
                databaseName,
                retrievalQuery,
                awaitIndexTimeout);
    }
}
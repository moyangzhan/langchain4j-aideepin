package com.moyz.adi.common.rag.neo4j;

import com.moyz.adi.common.rag.AdiEmbeddingSearchRequest;
import dev.langchain4j.data.embedding.Embedding;
import dev.langchain4j.data.segment.TextSegment;
import dev.langchain4j.store.embedding.EmbeddingMatch;
import dev.langchain4j.store.embedding.EmbeddingSearchRequest;
import dev.langchain4j.store.embedding.EmbeddingSearchResult;
import dev.langchain4j.store.embedding.EmbeddingStore;
import dev.langchain4j.store.embedding.filter.Filter;
import dev.langchain4j.store.embedding.neo4j.Neo4jEmbeddingStore;
import org.apache.commons.collections4.CollectionUtils;
import org.neo4j.driver.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.stream.Stream;

import static com.moyz.adi.common.rag.neo4j.Neo4jEmbeddingUtils.*;
import static dev.langchain4j.internal.Utils.*;
import static dev.langchain4j.internal.ValidationUtils.*;
import static java.util.Collections.singletonList;

/**
 * Represents a <a href="https://neo4j.com/">Neo4j</a> Vector index as an embedding store.
 * <p>
 * Instances of this store are created by configuring a builder:
 * <p><pre>{@code
 * EmbeddingStore<TextSegment> example() {
 *   return Neo4jEmbeddingStore.builder()
 *             .withBasicAuth("bolt://host:port", "username", "password")
 *             .dimension(384)
 *             .build();
 * }
 * }</pre>
 */
public class AdiNeo4jEmbeddingStore implements EmbeddingStore<TextSegment> {

    private static final Logger log = LoggerFactory.getLogger(Neo4jEmbeddingStore.class);
    public static final String ENTITIES_CREATION =
            """
                    UNWIND $rows AS row
                    MERGE (u:%1$s {%2$s: row.%2$s})
                    SET u += row.%3$s
                    WITH row, u
                    CALL db.create.setNodeVectorProperty(u, $embeddingProperty, row.%4$s)
                    RETURN count(*)""";
    public static final String INDEX_ALREADY_EXISTS_ERROR =
            """
                    It's not possible to create an index for the label `%s` and the property `%s`,
                    as there is another index with name `%s` with different labels: `%s` and properties `%s`.
                    Please provide another indexName to create the vector index, or delete the existing one""";
    public static final String CREATE_VECTOR_INDEX =
            """
                    CREATE VECTOR INDEX %s IF NOT EXISTS
                    FOR (m:%s) ON m.%s
                    OPTIONS { indexConfig: {
                        `vector.dimensions`: %s,
                        `vector.similarity_function`: 'cosine'
                    }}
                    """;

    /* Neo4j Java Driver settings */
    private final Driver driver;
    private final SessionConfig config;

    /* Neo4j schema field settings */
    private final int dimension;
    private final long awaitIndexTimeout;

    private final String indexName;
    private final String metadataPrefix;
    private final String embeddingProperty;
    private final String sanitizedEmbeddingProperty;
    private final String idProperty;
    private final String sanitizedIdProperty;
    private final String label;
    private final String sanitizedLabel;
    private final String textProperty;
    private final String retrievalQuery;
    private final Set<String> notMetaKeys;

    /**
     * Creates an instance of Neo4jEmbeddingStore
     *
     * @param driver            the {@link Driver} (required)
     * @param dimension         the dimension (required)
     * @param config            the {@link SessionConfig}  (optional, default is `SessionConfig.forDatabase(`databaseName`)`)
     * @param label             the optional label name (default: "Document")
     * @param embeddingProperty the optional embeddingProperty name (default: "embedding")
     * @param idProperty        the optional id property name (default: "id")
     * @param metadataPrefix    the optional metadata prefix (default: "")
     * @param textProperty      the optional textProperty property name (default: "text")
     * @param indexName         the optional index name (default: "vector")
     * @param databaseName      the optional database name (default: "neo4j")
     * @param awaitIndexTimeout the optional awaiting timeout for all indexes to come online, in seconds (default: 60s)
     * @param retrievalQuery    the optional retrieval query
     *                          (default: "RETURN properties(node) AS metadata, node.`idProperty` AS `idProperty`, node.`textProperty` AS `textProperty`, node.`embeddingProperty` AS `embeddingProperty`, score")
     */
    public AdiNeo4jEmbeddingStore(
            SessionConfig config,
            Driver driver,
            int dimension,
            String label,
            String embeddingProperty,
            String idProperty,
            String metadataPrefix,
            String textProperty,
            String indexName,
            String databaseName,
            String retrievalQuery,
            long awaitIndexTimeout) {

        /* required configs */
        this.driver = ensureNotNull(driver, "driver");
        this.dimension = ensureBetween(dimension, 0, 4096, "dimension");

        /* optional configs */
        String dbName = getOrDefault(databaseName, DEFAULT_DATABASE_NAME);
        this.config = getOrDefault(config, SessionConfig.forDatabase(dbName));
        this.label = getOrDefault(label, DEFAULT_LABEL);
        this.embeddingProperty = getOrDefault(embeddingProperty, DEFAULT_EMBEDDING_PROP);
        this.idProperty = getOrDefault(idProperty, DEFAULT_ID_PROP);
        this.indexName = getOrDefault(indexName, DEFAULT_IDX_NAME);
        this.metadataPrefix = getOrDefault(metadataPrefix, "");
        this.textProperty = getOrDefault(textProperty, DEFAULT_TEXT_PROP);
        this.awaitIndexTimeout = getOrDefault(awaitIndexTimeout, DEFAULT_AWAIT_INDEX_TIMEOUT);

        /* sanitize labels and property names, to prevent from Cypher Injections */
        this.sanitizedLabel = sanitizeOrThrows(this.label, "label");
        this.sanitizedEmbeddingProperty = sanitizeOrThrows(this.embeddingProperty, "embeddingProperty");
        this.sanitizedIdProperty = sanitizeOrThrows(this.idProperty, "idProperty");
        String sanitizedText = sanitizeOrThrows(this.textProperty, "textProperty");

        /* retrieval query: must necessarily return the following column:
            `metadata`,
            `score`,
            `this.idProperty (default "id")`,
            `this.textProperty (default "textProperty")`,
            `this.embeddingProperty (default "embedding")`
        */
        String defaultRetrievalQuery = String.format(
                "RETURN properties(node) AS metadata, node.%1$s AS %1$s, node.%2$s AS %2$s, node.%3$s AS %3$s, score",
                this.sanitizedIdProperty, sanitizedText, sanitizedEmbeddingProperty);
        this.retrievalQuery = getOrDefault(retrievalQuery, defaultRetrievalQuery);

        this.notMetaKeys = new HashSet<>(Arrays.asList(this.idProperty, this.embeddingProperty, this.textProperty));

        /* auto-schema creation */
        createSchema();
    }

    public static Builder builder() {
        return new Builder();
    }

    /*
    Getter methods
    */
    public Set<String> getNotMetaKeys() {
        return notMetaKeys;
    }

    public String getMetadataPrefix() {
        return metadataPrefix;
    }

    public String getTextProperty() {
        return textProperty;
    }

    public String getIdProperty() {
        return idProperty;
    }

    public String getEmbeddingProperty() {
        return embeddingProperty;
    }

    public String getIndexName() {
        return indexName;
    }

    /*
    Methods with `@Override`
    */

    @Override
    public String add(Embedding embedding) {
        String id = randomUUID();
        add(id, embedding);
        return id;
    }

    @Override
    public void add(String id, Embedding embedding) {
        addInternal(id, embedding, null);
    }

    @Override
    public String add(Embedding embedding, TextSegment textSegment) {
        String id = randomUUID();
        addInternal(id, embedding, textSegment);
        return id;
    }

    @Override
    public List<String> addAll(List<Embedding> embeddings) {
        return addAll(embeddings, null);
    }

    @Override
    public void removeAll() {
        try (var session = session()) {
            String statement = String.format(
                    "CALL { MATCH (n:%1$s) WHERE n.%2$s IS NOT NULL AND size(n.%2$s) = toInteger(%3$s) DETACH DELETE n } IN TRANSACTIONS",
                    this.sanitizedLabel, this.embeddingProperty, this.dimension);
            session.run(statement);
        }
    }

    @Override
    public void removeAll(Collection<String> ids) {
        ensureNotEmpty(ids, "ids");

        try (var session = session()) {
            String statement = String.format(
                    "CALL { UNWIND $ids AS id MATCH (n:%1$s {%2$s: id}) WHERE n.%3$s IS NOT NULL AND size(n.%3$s) = toInteger(%4$s) DETACH DELETE n } IN TRANSACTIONS ",
                    this.sanitizedLabel, this.sanitizedIdProperty, this.embeddingProperty, this.dimension);
            final Map<String, Object> params = Map.of("ids", ids);
            session.run(statement, params);
        }
    }

    @Override
    public void removeAll(Filter filter) {
        ensureNotNull(filter, "filter");

        final AbstractMap.SimpleEntry<String, Map<?, ?>> filterEntry = new Neo4jFilterMapper().map(filter);

        try (var session = session()) {
            String statement = String.format(
                    "CALL { MATCH (n:%1$s) WHERE n.%2$s IS NOT NULL AND size(n.%2$s) = toInteger(%3$s) AND %4$s DETACH DELETE n } IN TRANSACTIONS ",
                    this.sanitizedLabel, this.embeddingProperty, this.dimension, filterEntry.getKey());
            final Map params = filterEntry.getValue();
            session.run(statement, params);
        }
    }

    @Override
    public EmbeddingSearchResult<TextSegment> search(EmbeddingSearchRequest request) {

        var embeddingValue = Values.value(request.queryEmbedding().vector());

        try (var session = session()) {
            final Filter filter = request.filter();
            if (filter == null) {
                return getSearchResUsingVectorIndex(request, embeddingValue, session);
            }
            return getSearchResUsingVectorSimilarity(request, filter, embeddingValue, session);
        }
    }

    public EmbeddingSearchResult<TextSegment> searchByIds(List<String> ids) {
        Map<String, Object> params = Map.of();
        try (var session = session()) {
            String query = """
                    CALL db.index.vector.queryNodes($indexName, $maxResults, $embeddingValue)
                    YIELD node
                    WHERE elementId(node) in (%s)
                    """.formatted(String.join(",", ids));
            return getEmbeddingSearchResult(session, query, params);
        }
    }

    public EmbeddingSearchResult<TextSegment> searchByMetadata(Filter filter, int maxResult) {
        try (var session = session()) {
            final AbstractMap.SimpleEntry<String, Map<?, ?>> entry = new Neo4jFilterMapper().map(filter);
            String query = """
                    CYPHER runtime = parallel parallelRuntimeSupport=all
                    MATCH (n:%1$s)
                    WHERE n.%2$s IS NOT NULL AND %3$s
                    WITH n, 1 AS score
                    WITH n AS node,score
                    ORDER BY elementId(n)
                    LIMIT $maxResults
                    """
                    .formatted(
                            this.sanitizedLabel,
                            this.embeddingProperty,
                            entry.getKey());
            final Map params = entry.getValue();
            params.put("maxResults", maxResult);
            return getEmbeddingSearchResult(session, query, params);
        }
    }

    /*
    Private methods
    */
    private EmbeddingSearchResult getSearchResUsingVectorSimilarity(
            EmbeddingSearchRequest request, Filter filter, Value embeddingValue, Session session) {
        final AbstractMap.SimpleEntry<String, Map<?, ?>> entry = new Neo4jFilterMapper().map(filter);
        String query =
                """
                        CYPHER runtime = parallel parallelRuntimeSupport=all
                        MATCH (n:%1$s)
                        WHERE n.%2$s IS NOT NULL AND size(n.%2$s) = toInteger(%3$s) AND %4$s
                        WITH n, vector.similarity.cosine(n.%2$s, %5$s) AS score
                        WHERE score >= $minScore
                        WITH n AS node, score
                        ORDER BY score DESC
                        LIMIT $maxResults
                        """
                        .formatted(
                                this.sanitizedLabel,
                                this.embeddingProperty,
                                this.dimension,
                                entry.getKey(),
                                embeddingValue);
        final Map params = entry.getValue();
        params.put("minScore", request.minScore());
        params.put("maxResults", request.maxResults());
        return getEmbeddingSearchResult(session, query, params);
    }

    private EmbeddingSearchResult<TextSegment> getSearchResUsingVectorIndex(
            EmbeddingSearchRequest request, Value embeddingValue, Session session) {
        Map<String, Object> params = Map.of(
                "indexName",
                indexName,
                "embeddingValue",
                embeddingValue,
                "minScore",
                request.minScore(),
                "maxResults",
                request.maxResults());

        String query =
                """
                        CALL db.index.vector.queryNodes($indexName, $maxResults, $embeddingValue)
                        YIELD node, score
                        WHERE score >= $minScore
                        """;
        return getEmbeddingSearchResult(session, query, params);
    }

    private EmbeddingSearchResult<TextSegment> getEmbeddingSearchResult(
            Session session, String query, Map<String, Object> params) {
        List<EmbeddingMatch<TextSegment>> matches =
                session.run(query + retrievalQuery, params).list(item -> toEmbeddingMatch(this, item));

        return new EmbeddingSearchResult<>(matches);
    }

    private void addInternal(String id, Embedding embedding, TextSegment embedded) {
        addAll(singletonList(id), singletonList(embedding), embedded == null ? null : singletonList(embedded));
    }

    @Override
    public void addAll(List<String> ids, List<Embedding> embeddings, List<TextSegment> embedded) {
        if (isNullOrEmpty(ids) || isNullOrEmpty(embeddings)) {
            log.info("[do not add empty embeddings to neo4j]");
            return;
        }
        ensureTrue(ids.size() == embeddings.size(), "ids size is not equal to embeddings size");
        ensureTrue(
                embedded == null || embeddings.size() == embedded.size(),
                "embeddings size is not equal to embedded size");

        bulk(ids, embeddings, embedded);
    }

    private void bulk(List<String> ids, List<Embedding> embeddings, List<TextSegment> embedded) {
        Stream<List<Map<String, Object>>> rowsBatched = getRowsBatched(this, ids, embeddings, embedded);

        try (var session = session()) {
            rowsBatched.forEach(rows -> {
                String statement = String.format(
                        ENTITIES_CREATION, this.sanitizedLabel, this.sanitizedIdProperty, PROPS, EMBEDDINGS_ROW_KEY);

                Map<String, Object> params = Map.of("rows", rows, "embeddingProperty", this.embeddingProperty);

                session.executeWrite(tx -> tx.run(statement, params).consume());
            });
        }
    }

    private void createSchema() {
        if (!indexExists()) {
            createIndex();
        }
        createUniqueConstraint();
    }

    private void createUniqueConstraint() {
        try (var session = session()) {
            String query = String.format(
                    "CREATE CONSTRAINT IF NOT EXISTS FOR (n:%s) REQUIRE n.%s IS UNIQUE",
                    this.sanitizedLabel, this.sanitizedIdProperty);
            session.run(query);
        }
    }

    private boolean indexExists() {
        try (var session = session()) {
            Map<String, Object> params = Map.of("name", this.indexName);
            var resIndex = session.run("SHOW VECTOR INDEX WHERE name = $name", params);
            if (!resIndex.hasNext()) {
                return false;
            }
            var record = resIndex.single();
            List<String> idxLabels = record.get("labelsOrTypes").asList(Value::asString);
            List<Object> idxProps = record.get("properties").asList();

            boolean isIndexDifferent = !idxLabels.equals(singletonList(this.label))
                                       || !idxProps.equals(singletonList(this.embeddingProperty));
            if (isIndexDifferent) {
                String errMessage = String.format(
                        INDEX_ALREADY_EXISTS_ERROR,
                        this.label,
                        this.embeddingProperty,
                        this.indexName,
                        idxLabels,
                        idxProps);
                throw new RuntimeException(errMessage);
            }
            return true;
        }
    }

    private void createIndex() {
        Map<String, Object> params = Map.of(
                "indexName",
                this.indexName,
                "label",
                this.label,
                "embeddingProperty",
                this.embeddingProperty,
                "dimension",
                this.dimension);

        // create vector index
        try (var session = session()) {
            final String createIndexQuery = String.format(
                    CREATE_VECTOR_INDEX, indexName, sanitizedLabel, sanitizedEmbeddingProperty, dimension);
            session.run(createIndexQuery, params);

            session.run("CALL db.awaitIndexes($timeout)", Map.of("timeout", awaitIndexTimeout))
                    .consume();
        }
    }

    private Session session() {
        return this.driver.session(this.config);
    }
}
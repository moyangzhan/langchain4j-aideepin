package com.moyz.adi.common.rag.neo4j;

import dev.langchain4j.community.store.embedding.neo4j.Neo4jEmbeddingStore;
import dev.langchain4j.data.embedding.Embedding;
import dev.langchain4j.data.segment.TextSegment;
import dev.langchain4j.store.embedding.EmbeddingMatch;
import dev.langchain4j.store.embedding.EmbeddingSearchRequest;
import dev.langchain4j.store.embedding.EmbeddingSearchResult;
import dev.langchain4j.store.embedding.EmbeddingStore;
import dev.langchain4j.store.embedding.filter.Filter;
import dev.langchain4j.store.embedding.filter.comparison.IsIn;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.neo4j.cypherdsl.core.Condition;
import org.neo4j.cypherdsl.core.Cypher;
import org.neo4j.cypherdsl.core.Node;
import org.neo4j.cypherdsl.core.Statement;
import org.neo4j.cypherdsl.core.renderer.Renderer;
import org.neo4j.driver.Driver;
import org.neo4j.driver.Session;
import org.neo4j.driver.SessionConfig;

import java.lang.reflect.Field;
import java.util.*;

import static com.moyz.adi.common.rag.neo4j.Neo4jEmbeddingUtils.toEmbeddingMatch;
import static org.neo4j.cypherdsl.core.Cypher.*;


@Slf4j
public class AdiNeo4jEmbeddingStore implements EmbeddingStore<TextSegment> {

    private final Driver driver;
    private final SessionConfig config;
    private final String sanitizedLabel;
    private final String embeddingProperty;
    private final String idProperty;

    private final Neo4jEmbeddingStore neo4jEmbeddingStore;

    public AdiNeo4jEmbeddingStore(Neo4jEmbeddingStore neo4jEmbeddingStore) {
        this.neo4jEmbeddingStore = neo4jEmbeddingStore;
        try {
            Field field1 = FieldUtils.getDeclaredField(Neo4jEmbeddingStore.class, "driver", true);
            Field field2 = FieldUtils.getDeclaredField(Neo4jEmbeddingStore.class, "config", true);
            Field field3 = FieldUtils.getDeclaredField(Neo4jEmbeddingStore.class, "sanitizedLabel", true);
            Field field4 = FieldUtils.getDeclaredField(Neo4jEmbeddingStore.class, "embeddingProperty", true);
            Field field5 = FieldUtils.getDeclaredField(Neo4jEmbeddingStore.class, "idProperty", true);
            this.driver = (Driver) FieldUtils.readField(field1, neo4jEmbeddingStore);
            this.config = (SessionConfig) FieldUtils.readField(field2, neo4jEmbeddingStore);
            this.sanitizedLabel = (String) FieldUtils.readField(field3, neo4jEmbeddingStore);
            this.embeddingProperty = (String) FieldUtils.readField(field4, neo4jEmbeddingStore);
            this.idProperty = (String) FieldUtils.readField(field5, neo4jEmbeddingStore);
        } catch (IllegalAccessException e) {
            throw new RuntimeException(e);
        }
    }

    public EmbeddingSearchResult<TextSegment> searchByIds(List<String> ids) {
        if (ids.isEmpty()) {
            log.warn("searchByIds ids is empty");
            return new EmbeddingSearchResult<>(new ArrayList<>());
        }
        Map<String, Object> params = new HashMap<>();
        params.put("maxResults", 1000);
        try (var session = session()) {
//            String query = """
//                    match (node:%s)
//                    WHERE elementId(node) in (%s)
//                    with node
//                    return properties(node) as metadata, elementId(node) as elementId, node.text as text, node.embedding as embedding, 1 as score
//                    """.formatted(this.sanitizedLabel, String.join(",", ids));
            Node node = node(this.sanitizedLabel).named("node");
            AdiNeo4jFilterMapper neo4jFilterMapper = new AdiNeo4jFilterMapper(node);
            Condition condition = node.property(this.embeddingProperty)
                    .isNotNull()
                    .and(neo4jFilterMapper.getCondition(new IsIn("id", ids)));
            Statement statement = match(node)
                    .where(condition)
                    .with(node)
                    .returning(Cypher.raw("properties(node) as metadata, node['id'] as id, node.text as text, node.embedding as embedding, 1 as score"))
                    .orderBy(name(idProperty))
                    .descending()
                    .limit(parameter("maxResults"))
                    .build();
            String cypherQuery = Renderer.getDefaultRenderer().render(statement);
            log.info("searchByIds cypherQuery: {}", cypherQuery);
            return getEmbeddingSearchResult(session, cypherQuery, params);
        }
    }

    public int countByMetadata(Filter filter) {
        try (var session = session()) {
            Node node = node(this.sanitizedLabel).named("node");
            AdiNeo4jFilterMapper neo4jFilterMapper = new AdiNeo4jFilterMapper(node);
            Condition condition = node.property(this.embeddingProperty)
                    .isNotNull()
                    .and(neo4jFilterMapper.getCondition(filter));
            Statement statement = match(node)
                    .where(condition)
                    .with(node)
                    .returning(Cypher.raw("count(node) as count"))
                    .build();
            String cypherQuery = Renderer.getDefaultRenderer().render(statement);
            log.info("countByMetadata cypherQuery: {}", cypherQuery);
            return session.run(cypherQuery).single().get("count").asInt();
        }
    }

    public EmbeddingSearchResult<TextSegment> searchByMetadata(Filter filter, int maxResult) {
        try (var session = session()) {
            Node node = node(this.sanitizedLabel).named("node");
            AdiNeo4jFilterMapper neo4jFilterMapper = new AdiNeo4jFilterMapper(node);
            Condition condition = node.property(this.embeddingProperty)
                    .isNotNull()
                    .and(neo4jFilterMapper.getCondition(filter));
            Statement statement = match(node)
                    .where(condition)
                    .with(node)
                    .returning(Cypher.raw("properties(node) as metadata, node['id'] as id, node.text as text, node.embedding as embedding, 1 as score"))
                    .orderBy(name(idProperty))
                    .descending()
                    .limit(parameter("maxResults"))
                    .build();
            String cypherQuery = Renderer.getDefaultRenderer().render(statement);
            log.info("searchByMetadata cypherQuery: {}", cypherQuery);
            Map<String, Object> params = new HashMap<>();
            params.put("maxResults", maxResult);
            return getEmbeddingSearchResult(session, cypherQuery, params);
        }
    }

    /**
     * Timeline retrieval: match nodes satisfying {@code filter} and order by a
     * metadata property (e.g. {@code create_time}) descending. Mirrors the SQL
     * {@code order by metadata ->> 'create_time' desc} used by the pgvector
     * backend so callers behave identically across stores.
     * <p>
     * 时间轴检索：按 filter 匹配，再按 metadata 字段（如 create_time）倒序。与 pgvector 后端
     * 的 SQL {@code order by metadata ->> 'create_time' desc} 行为一致。
     */
    public EmbeddingSearchResult<TextSegment> searchByMetadataOrdered(Filter filter, String orderByProperty, int maxResult) {
        // orderByProperty must come from a server-side constant (e.g. MetadataKey.CREATE_TIME).
        // Reject anything that isn't a plain identifier so a future caller accidentally passing
        // user input cannot inject Cypher via this string.
        // <p>
        // orderByProperty 必须来自服务端常量。白名单兜底：拒绝非纯标识符的输入，防止
        // 未来调用者误传用户输入导致 Cypher 注入。
        if (orderByProperty == null || !orderByProperty.matches("[A-Za-z_][A-Za-z0-9_]*")) {
            throw new IllegalArgumentException("Invalid orderByProperty: " + orderByProperty);
        }
        try (var session = session()) {
            Node node = node(this.sanitizedLabel).named("node");
            AdiNeo4jFilterMapper neo4jFilterMapper = new AdiNeo4jFilterMapper(node);
            Condition condition = node.property(this.embeddingProperty)
                    .isNotNull()
                    .and(neo4jFilterMapper.getCondition(filter));
            // Inline the property name as a literal — dynamic-property access via
            // node[$param] depends on Neo4j 5.x; literal access (node.create_time)
            // is supported across versions.
            // <p>
            // 用字面属性访问（node.create_time），避免 node[$param] 动态访问的版本依赖。
            Statement statement = match(node)
                    .where(condition)
                    .with(node)
                    .returning(Cypher.raw("properties(node) as metadata, node['id'] as id, node.text as text, node.embedding as embedding, 1 as score"))
                    .orderBy(Cypher.raw("node.`" + orderByProperty + "`"))
                    .descending()
                    .limit(parameter("maxResults"))
                    .build();
            String cypherQuery = Renderer.getDefaultRenderer().render(statement);
            log.info("searchByMetadataOrdered cypherQuery: {}", cypherQuery);
            Map<String, Object> params = new HashMap<>();
            params.put("maxResults", maxResult);
            return getEmbeddingSearchResult(session, cypherQuery, params);
        }
    }

    private EmbeddingSearchResult<TextSegment> getEmbeddingSearchResult(
            Session session, String query, Map<String, Object> params) {
        List<EmbeddingMatch<TextSegment>> matches =
                session.run(query, params).list(item -> toEmbeddingMatch(this, item));

        return new EmbeddingSearchResult<>(matches);
    }

    private Session session() {
        return this.driver.session(this.config);
    }


    public Neo4jEmbeddingStore getNeo4jEmbeddingStore() {
        return neo4jEmbeddingStore;
    }

    @Override
    public String add(Embedding embedding) {
        return neo4jEmbeddingStore.add(embedding);
    }

    @Override
    public void add(String s, Embedding embedding) {
        neo4jEmbeddingStore.add(s, embedding);
    }

    @Override
    public String add(Embedding embedding, TextSegment textSegment) {
        return neo4jEmbeddingStore.add(embedding, textSegment);
    }

    @Override
    public List<String> addAll(List<Embedding> list) {
        return neo4jEmbeddingStore.addAll(list);
    }

    @Override
    public List<String> addAll(List<Embedding> embeddings, List<TextSegment> embedded) {
        return neo4jEmbeddingStore.addAll(embeddings, embedded);
    }

    @Override
    public void addAll(List<String> ids, List<Embedding> embeddings, List<TextSegment> embedded) {
        neo4jEmbeddingStore.addAll(ids, embeddings, embedded);
    }

    @Override
    public EmbeddingSearchResult<TextSegment> search(EmbeddingSearchRequest embeddingSearchRequest) {
        return neo4jEmbeddingStore.search(embeddingSearchRequest);
    }

    @Override
    public void removeAll(Filter filter) {
        neo4jEmbeddingStore.removeAll(filter);
    }

    @Override
    public void remove(String id) {
        neo4jEmbeddingStore.remove(id);
    }

    @Override
    public void removeAll() {
        neo4jEmbeddingStore.removeAll();
    }

    @Override
    public void removeAll(Collection<String> ids) {
        neo4jEmbeddingStore.removeAll(ids);
    }
}
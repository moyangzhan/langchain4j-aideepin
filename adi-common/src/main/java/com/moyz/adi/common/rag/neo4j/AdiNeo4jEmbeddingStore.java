package com.moyz.adi.common.rag.neo4j;

import dev.langchain4j.community.store.embedding.neo4j.Neo4jEmbeddingStore;
import dev.langchain4j.community.store.embedding.neo4j.Neo4jFilterMapper;
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
            Neo4jFilterMapper neo4jFilterMapper = new Neo4jFilterMapper(node);
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
            Neo4jFilterMapper neo4jFilterMapper = new Neo4jFilterMapper(node);
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
            Neo4jFilterMapper neo4jFilterMapper = new Neo4jFilterMapper(node);
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
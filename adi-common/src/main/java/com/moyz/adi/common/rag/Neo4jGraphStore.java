package com.moyz.adi.common.rag;

import com.google.common.base.Joiner;
import com.moyz.adi.common.rag.neo4j.Neo4jFilterMapper;
import com.moyz.adi.common.util.AdiStringUtil;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.vo.*;
import dev.langchain4j.store.graph.neo4j.Neo4jGraph;
import lombok.Builder;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.commons.lang3.tuple.Triple;
import org.neo4j.driver.Record;
import org.neo4j.driver.*;
import org.neo4j.driver.exceptions.ClientException;
import org.neo4j.driver.exceptions.Neo4jException;
import org.neo4j.driver.internal.value.BooleanValue;
import org.neo4j.driver.internal.value.FloatValue;
import org.neo4j.driver.internal.value.StringValue;
import org.neo4j.driver.types.Node;
import org.neo4j.driver.types.Relationship;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

import static com.moyz.adi.common.cosntant.AdiConstant.GRAPH_STORE_MAIN_FIELDS;
import static dev.langchain4j.internal.ValidationUtils.*;

public class Neo4jGraphStore implements GraphStore {
    private static final Logger log = LoggerFactory.getLogger(Neo4jGraphStore.class);

    private final String host;

    private final Integer port;

    private final String user;

    private final String password;

    private final String graphName;

    private final Driver driver;

    private final Neo4jGraph neo4jGraph;

    @Builder
    public Neo4jGraphStore(String host,
                           Integer port,
                           String user,
                           String password,
                           String graphName,
                           Boolean dropGraphFirst) {
        this.host = ensureNotBlank(host, "host");
        this.port = ensureGreaterThanZero(port, "port");
        this.user = ensureNotBlank(user, "user");
        this.password = ensureNotBlank(password, "password");
        this.graphName = graphName;
        driver = GraphDatabase.driver("neo4j://" + this.host + ":" + this.port, AuthTokens.basic(this.user, this.password));
        neo4jGraph = new AdiNeo4jGraph(driver);
        if (Boolean.TRUE.equals(dropGraphFirst)) {
            try (Session session = this.driver.session()) {
                session.executeWrite(tx -> tx.run(String.format("MATCH (n:%s) DELETE n", this.graphName)));
            } catch (ClientException e) {
                throw new Neo4jException("Error executing drop graph", e);
            }
        }
    }

    @Override
    public boolean addVertexes(List<GraphVertex> vertexes) {
        ensureNotEmpty(vertexes, vertexes.toString());
        try (Session session = driver.session()) {
            session.executeWrite(tx -> {
                for (GraphVertex vertex : vertexes) {
                    Map<String, Object> metadata = vertex.getMetadata();
                    String label = StringUtils.isNotBlank(vertex.getLabel()) ? ":" + vertex.getLabel() : "";
                    Pair<String, Map<String, Object>> causeToArgs = buildCauseByMetadata(metadata, true);
                    String sql;
                    if (StringUtils.isNotBlank(causeToArgs.getLeft())) {
                        sql = ("""
                                create (:%s%s {name:$name,text_segment_id:$text_segment_id,description:$description,%s})
                                """).formatted(graphName, label, causeToArgs.getLeft());
                    } else {
                        sql = ("""
                                create (:%s%s {name:$name,text_segment_id:$text_segment_id,description:$description})
                                """).formatted(graphName, label);
                    }
                    log.info("addVertexes,sql:{}", sql);
                    Map<String, Object> params = new HashMap<>();
                    params.put("name", AdiStringUtil.tail(vertex.getName(), 20));
                    params.put("text_segment_id", null == vertex.getTextSegmentId() ? "" : vertex.getTextSegmentId());
                    params.put("description", vertex.getDescription());
                    params.putAll(causeToArgs.getRight());
                    tx.run(sql, params);
                }
                return null;
            });
        }
        return true;
    }

    @Override
    public boolean addVertex(GraphVertex vertex) {
        log.info("Add vertex:{}", vertex);
        ensureNotNull(vertex, vertex.toString());
        ensureNotEmpty(vertex.getMetadata(), "Metadata");
        return addVertexes(List.of(vertex));
    }

    @Override
    public GraphVertex updateVertex(GraphVertexUpdateInfo updateInfo) {
        log.info("Update vertex:{}", updateInfo.getNewData());
        ensureNotNull(updateInfo.getMetadataFilter(), "Metadata filter");
        GraphVertex newData = updateInfo.getNewData();
        ensureNotNull(newData, newData.toString());
        try (Session session = driver.session()) {
            List<Record> records = session.executeWrite(tx -> {
                GraphSearchCondition whereCondition = GraphSearchCondition.builder()
                        .names(List.of(updateInfo.getName()))
                        .metadataFilter(updateInfo.getMetadataFilter())
                        .build();
                String alias = "v";
                Pair<String, Map<String, Object>> whereClause = buildWhereClause(whereCondition, "v");
                Pair<String, Map<String, Object>> causeToArgs = buildCauseByMetadata(updateInfo.getNewData().getMetadata(), alias, false);

                String prepareSql;
                if (StringUtils.isNotBlank(causeToArgs.getLeft())) {
                    prepareSql = """
                               match (v:%1$s)
                               where %2$s
                               set v.text_segment_id=$new_text_segment_id,v.description=$new_description,%3$s
                               return v
                               limit 1
                            """.formatted(graphName, whereClause.getLeft(), causeToArgs.getLeft());
                } else {
                    prepareSql = """
                               match (v:%s)
                               where %s
                               set v.text_segment_id=$new_text_segment_id,v.description=$new_description
                               return v
                               limit 1
                            """.formatted(graphName, whereClause.getLeft());
                }
                log.info("updateVertex prepareSql:{}", prepareSql);
                Map<String, Object> params = new HashMap<>();
                params.put("new_text_segment_id", newData.getTextSegmentId());
                params.put("new_description", newData.getDescription());
                params.putAll(whereClause.getRight());
                params.putAll(causeToArgs.getRight());
                return tx.run(prepareSql, params).list();
            });
            return getVertexFromResultSet(records);
        }
    }

    @Override
    public GraphVertex getVertex(GraphVertexSearch search) {
        List<GraphVertex> list = this.searchVertices(search);
        if (list.isEmpty()) {
            return null;
        }
        return list.get(0);
    }

    @Override
    public List<GraphVertex> getVertices(List<String> ids) {
        String query = """
                    match (v:%s)
                    where elementId(v) in [%s]
                    return v
                """.formatted(graphName, Joiner.on(",").join(ids));
        List<Record> records = neo4jGraph.executeRead(query);
        log.info("getVertices query:{}", query);
        return getVerticesFromResultSet(records);
    }

    @Override
    public List<GraphVertex> searchVertices(GraphVertexSearch search) {
        String label = search.getLabel();
        Neo4jFilterMapper neo4jFilterMapper = new Neo4jFilterMapper("v");
        Pair<String, Map<String, Object>> filterClause = buildWhereClause(search, "v");
        //TODO and elementId(v) < %d ,search.getMaxId(),
        String query = """
                    match (v:%s%s)
                    with v
                    order by elementId(v) desc
                    where %s
                    return v
                    limit %d
                """.formatted(graphName, StringUtils.isNotBlank(label) ? ":" + label : "", filterClause.getLeft(), search.getLimit());
        log.info("SearchVertices prepareSql:{}", query);
        try (Session session = driver.session()) {
            Map<String, Object> params = new HashMap<>();
            params.putAll(neo4jFilterMapper.getIncrementalKeyMap().getMap());
            params.putAll(filterClause.getRight());
            List<Record> records = session.executeRead(tx -> tx.run(query, params).list());
            return getVerticesFromResultSet(records);
        }
    }

    @Override
    public List<Triple<GraphVertex, GraphEdge, GraphVertex>> getEdges(List<String> ids) {
        String query = """
                    match (v1)-[e:%s]->(v2)
                    where elementId(e) in [%s]
                    return v1,e,v2
                """.formatted(graphName, Joiner.on(",").join(ids));
        log.info("getEdges query:{}", query);
        List<Record> records = neo4jGraph.executeRead(query);
        return getEdgesFromResultSet(records);
    }

    @Override
    public List<Triple<GraphVertex, GraphEdge, GraphVertex>> searchEdges(GraphEdgeSearch search) {
        log.info("searchEdges:{}", search);
        try (Session session = driver.session()) {
            Pair<String, Map<String, Object>> filterClause1 = buildWhereClause(search.getSource(), "v1");
            Pair<String, Map<String, Object>> filterClause2 = buildWhereClause(search.getTarget(), "v2");
            Pair<String, Map<String, Object>> filterClause3 = buildWhereClause(search.getEdge(), "e");
            StringBuilder filterClause = new StringBuilder();
            appendSql(filterClause, filterClause1);
            appendSql(filterClause, filterClause2);
            appendSql(filterClause, filterClause3);
            //TODO and elementId(e) < %d  search.getMaxId(),
            String query = """
                        match (v1)-[e:%s]-(v2)
                        with v1,e,v2
                        order by elementId(e) desc
                        where %s
                        return v1,e,v2
                        limit %d
                    """.formatted(graphName, filterClause.toString(), search.getLimit());
            log.info("Search edges prepareSql:\n{}", query);
            List<Record> records = session.executeRead(tx -> {
                Map<String, Object> whereArgs1 = filterClause1.getRight();
                Map<String, Object> whereArgs2 = filterClause2.getRight();
                Map<String, Object> whereArgs3 = filterClause3.getRight();
                whereArgs1.putAll(whereArgs2);
                whereArgs1.putAll(whereArgs3);
                return tx.run(query, whereArgs1).list();
            });
            return getEdgesFromResultSet(records);
        }
    }

    public Triple<GraphVertex, GraphEdge, GraphVertex> getEdge(GraphEdgeSearch search) {
        List<Triple<GraphVertex, GraphEdge, GraphVertex>> list = this.searchEdges(search);
        if (list.isEmpty()) {
            return null;
        }
        return list.get(0);
    }

    public Triple<GraphVertex, GraphEdge, GraphVertex> addEdge(GraphEdgeAddInfo addInfo) {
        ensureNotNull(addInfo.getEdge(), "Grahp edge");
        try (Session session = driver.session()) {
            Pair<String, Map<String, Object>> filterClause1 = buildWhereClause(addInfo.getSourceFilter(), "v1");
            Pair<String, Map<String, Object>> filterClause2 = buildWhereClause(addInfo.getTargetFilter(), "v2");
            StringBuilder filterClause = new StringBuilder();
            appendSql(filterClause, filterClause1);
            appendSql(filterClause, filterClause2);
            String edgeAlias = "e";
            Pair<String, Map<String, Object>> clauseToArgs = buildCauseByMetadata(addInfo.getEdge().getMetadata(), edgeAlias, true);
            String query = """
                      match (v1:%1$s), (v2:%1$s)
                      where %2$s
                      create (v1)-[e:%1$s {text_segment_id:$text_segment_id,weight:$weight,description:$description,%3$s}]->(v2)
                      return v1,e,v2
                    """.formatted(graphName, filterClause.toString(), clauseToArgs.getLeft());
            log.info("Add edge prepareSql:{}", query);
            List<Record> records = session.executeWrite(tx -> {
                Map<String, Object> whereArgs1 = filterClause1.getRight();
                Map<String, Object> whereArgs2 = filterClause2.getRight();
                whereArgs1.putAll(whereArgs2);
                whereArgs1.putAll(JsonUtil.toMap(addInfo.getEdge()));
                whereArgs1.putAll(clauseToArgs.getRight());
                return tx.run(query, whereArgs1).list();
            });

            return getEdgeFromResultSet(records);
        }
    }

    public Triple<GraphVertex, GraphEdge, GraphVertex> updateEdge(GraphEdgeEditInfo edgeEditInfo) {
        log.info("Update edge:{}", edgeEditInfo);
        GraphEdge newData = edgeEditInfo.getEdge();
        ensureNotNull(newData, "Graph edit info");
        try (Session session = driver.session()) {
            String edgeAlias = "e";
            Pair<String, Map<String, Object>> filterClause1 = buildWhereClause(edgeEditInfo.getSourceFilter(), "v1");
            Pair<String, Map<String, Object>> filterClause2 = buildWhereClause(edgeEditInfo.getTargetFilter(), "v2");
            Pair<String, Map<String, Object>> causeToArgs = buildCauseByMetadata(newData.getMetadata(), edgeAlias, false);
            StringBuilder filterClause = new StringBuilder();
            appendSql(filterClause, filterClause1);
            appendSql(filterClause, filterClause2);
            String prepareSql = """
                       match (v1:%1$s)-[e:%1$s]->(v2:%1$s)
                       where %2$s
                       set e.weight=$new_weight,e.text_segment_id=$new_text_segment_id,e.description=$new_description %3$s
                       return v1,e,v2
                    """.formatted(graphName, filterClause, causeToArgs.getLeft());
            log.info("updateEdge prepareSql:{}", prepareSql);
            List<Record> records = session.executeWrite(tx -> {
                Map<String, Object> whereArgs1 = filterClause1.getRight();
                Map<String, Object> whereArgs2 = filterClause2.getRight();
                whereArgs1.putAll(whereArgs2);
                whereArgs1.putAll(JsonUtil.toMap(newData));
                whereArgs1.putAll(causeToArgs.getRight());
                return tx.run(prepareSql, whereArgs1).list();
            });
            return getEdgeFromResultSet(records);
        }
    }

    /**
     * 删除顶点(以及边)
     *
     * @param filter
     * @param includeEdges
     */
    public void deleteVertices(GraphSearchCondition filter, boolean includeEdges) {
        ensureNotNull(filter, "Data filter");
        ensureNotNull(filter.getMetadataFilter(), "Metadata filter");
        try (Session session = driver.session()) {
            Pair<String, Map<String, Object>> filterClause = buildWhereClause(filter, "v");
            String prepareSql = """
                      match (v:%s)
                      where %s %s
                    """.formatted(graphName, filterClause.getLeft(), includeEdges ? "DETACH DELETE v" : "DELETE v");
            log.info("deleteVertices prepareSql:{}", prepareSql);
            session.executeWrite(tx -> {
                Map<String, Object> whereArgs = filterClause.getRight();
                return tx.run(prepareSql, whereArgs);
            });
        }
    }

    /**
     * 单独删除边
     *
     * @param filter
     */
    public void deleteEdges(GraphSearchCondition filter) {
        ensureNotNull(filter, "Data filter");
        try (Session session = driver.session()) {
            Pair<String, Map<String, Object>> filterClause = buildWhereClause(filter, "e");
            String prepareSql = """
                        match (:%1$s)-[e:%1$s]->(%1$s)
                        where %2$s
                        delete e
                    """.formatted(graphName, filterClause.getLeft());
            log.info("deleteEdges prepareSql:{}", prepareSql);
            session.executeWrite(tx -> tx.run(prepareSql, filterClause.getRight()));
        }
    }

    private List<Triple<GraphVertex, GraphEdge, GraphVertex>> getEdgesFromResultSet(List<Record> records) {
        List<Triple<GraphVertex, GraphEdge, GraphVertex>> result = new ArrayList<>();
        for (Record record : records) {
            Node sourceNode = record.get("v1").asNode();
            Relationship relationship = record.get("e").asRelationship();
            Node targetNode = record.get("v2").asNode();
            result.add(Triple.of(agTypeToVertex(sourceNode), agTypeToEdge(sourceNode, relationship, targetNode), agTypeToVertex(targetNode)));
        }
        return result;
    }

    public Triple<GraphVertex, GraphEdge, GraphVertex> getEdgeFromResultSet(List<Record> records) {
        List<Triple<GraphVertex, GraphEdge, GraphVertex>> list = getEdgesFromResultSet(records);
        if (list.isEmpty()) {
            return null;
        }
        return list.get(0);
    }

    public GraphVertex getVertexFromResultSet(List<Record> records) {
        List<GraphVertex> vertices = getVerticesFromResultSet(records);
        if (vertices.isEmpty()) {
            return null;
        }
        return vertices.get(0);
    }

    public List<GraphVertex> getVerticesFromResultSet(List<Record> records) {
        List<GraphVertex> vertices = new ArrayList<>();
        for (Record record : records) {
            vertices.add(agTypeToVertex(record.get("v").asNode()));
        }
        return vertices;
    }

    public GraphVertex agTypeToVertex(Node node) {
        String label = node.get("label").toString();
        Map<String, Object> metadata = new HashMap<>();
        for (String key : node.keys()) {
            if (!GRAPH_STORE_MAIN_FIELDS.contains(key)) {
                Value value = node.get(key);
                if (value instanceof StringValue stringValue) {
                    metadata.put(key, stringValue.toString());
                } else if (value instanceof FloatValue floatValue) {
                    metadata.put(key, floatValue.asFloat());
                } else if (value instanceof BooleanValue booleanValue) {
                    metadata.put(key, booleanValue.asBoolean());
                } else {
                    metadata.put(key, value.toString());
                }
            }
        }
        return GraphVertex.builder()
                .id(node.elementId())
                .label(label)
                .name(node.get("name").asString())
                .description(node.get("description").asString())
                .textSegmentId(node.get("text_segment_id").asString())
                .metadata(metadata)
                .build();
    }

    private GraphEdge agTypeToEdge(Node sourceNode, Relationship relationship, Node targetNode) {
        String startId = sourceNode.elementId();
        String endId = targetNode.elementId();
        String nodeLabel = relationship.get("label").asString();
        Map<String, Object> metadata = new HashMap<>();
        for (String key : relationship.keys()) {
            if (!GRAPH_STORE_MAIN_FIELDS.contains(key)) {
                Value value = relationship.get(key);
                if (value instanceof StringValue stringValue) {
                    metadata.put(key, stringValue.toString());
                } else if (value instanceof FloatValue floatValue) {
                    metadata.put(key, floatValue.asFloat());
                } else if (value instanceof BooleanValue booleanValue) {
                    metadata.put(key, booleanValue.asBoolean());
                } else {
                    metadata.put(key, value.toString());
                }
            }
        }
        return GraphEdge.builder()
                .id(relationship.elementId())
                .startId(startId)
                .endId(endId)
                .label(nodeLabel)
                .weight(null == relationship.get("weight") ? 0 : relationship.get("weight").asDouble())
                .description(relationship.get("description").asString())
                .textSegmentId(relationship.get("text_segment_id").asString())
                .metadata(metadata)
                .build();
    }

    private Pair<String, Map<String, Object>> buildCauseByMetadata(Map<String, Object> metadata, String alias, boolean createCause) {
        String keyValueOperator = createCause ? ":" : "=";

        List<String> itemQuerySql = new ArrayList<>();
        Map<String, Object> argNameToVal = new HashMap<>();

        String tmpAlias = StringUtils.isNotBlank(alias) ? alias + "." : "";
        for (Map.Entry<String, Object> entry : metadata.entrySet()) {
            String argNamePrefix = StringUtils.isNotBlank(alias) ? alias + "_" : "";
            itemQuerySql.add((createCause ? "" : tmpAlias) + entry.getKey() + keyValueOperator + "$" + argNamePrefix + entry.getKey());

            argNameToVal.put(argNamePrefix + entry.getKey(), entry.getValue());
        }
        return new ImmutablePair<>(String.join(",", itemQuerySql), argNameToVal);
    }

    private Pair<String, Map<String, Object>> buildCauseByMetadata(Map<String, Object> metadata, boolean createCause) {
        return buildCauseByMetadata(metadata, "", createCause);
    }

    private Pair<String, Map<String, Object>> buildWhereClause(GraphSearchCondition search, String alias) {
        if (null == search) {
            return new ImmutablePair<>("", new HashMap<>());
        }
        StringBuilder whereCause = new StringBuilder();
        Map<String, Object> whereArgs = new HashMap<>();
        if (CollectionUtils.isNotEmpty(search.getNames())) {
            buildNamesClause(search.getNames(), whereCause, whereArgs, alias);
        }
        if (null != search.getMetadataFilter()) {
            if (!whereCause.isEmpty()) {
                whereCause.append(" and ");
            }
            Neo4jFilterMapper neo4jFilterMapper = new Neo4jFilterMapper(alias);
            final AbstractMap.SimpleEntry<String, Map<?, ?>> filterEntry = neo4jFilterMapper.map(search.getMetadataFilter());
            whereCause.append(filterEntry.getKey());
            whereArgs.putAll(neo4jFilterMapper.getIncrementalKeyMap().getMap());
        }
        return new ImmutablePair<>(whereCause.toString(), whereArgs);
    }

    private void buildNamesClause(List<String> names, StringBuilder whereClause, Map<String, Object> whereArgs, String alias) {
        if (names.isEmpty()) {
            return;
        }
        List<String> nameArgs = new ArrayList<>();
        for (int i = 0; i < names.size(); i++) {
            String argName = alias + "_name" + i;
            nameArgs.add("$" + argName);
            whereArgs.put(argName, names.get(i));
        }
        whereClause.append(String.format("(%s.name in [%s])", alias, String.join(",", nameArgs)));
    }

    private void appendSql(StringBuilder filterClause, Pair<String, Map<String, Object>> sqlAndArgs) {
        if (null == sqlAndArgs) {
            return;
        }
        String sql = sqlAndArgs.getLeft();
        if (!filterClause.isEmpty() && StringUtils.isNotBlank(sql)) {
            filterClause.append(" and ");
        }
        if (StringUtils.isNotBlank(sql)) {
            filterClause.append(sql);
        }
    }
}

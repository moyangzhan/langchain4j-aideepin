package com.moyz.adi.common.rag;

import com.google.common.base.Joiner;
import com.moyz.adi.common.util.GraphStoreUtil;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.vo.*;
import lombok.Builder;
import org.apache.age.jdbc.base.Agtype;
import org.apache.age.jdbc.base.type.AgtypeMap;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Triple;
import org.postgresql.jdbc.PgConnection;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.sql.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static dev.langchain4j.internal.Utils.getOrDefault;
import static dev.langchain4j.internal.ValidationUtils.*;

public class ApacheAgeGraphStore {

    private static final Logger log = LoggerFactory.getLogger(ApacheAgeGraphStore.class);

    private final String host;
    private final Integer port;
    private final String user;
    private final String password;
    private final String database;
    private final String graph;

    @Builder
    public ApacheAgeGraphStore(String host,
                               Integer port,
                               String user,
                               String password,
                               String database,
                               String graphName,
                               Boolean createGraph,
                               Boolean dropGraphFirst) {
        this.host = ensureNotBlank(host, "host");
        this.port = ensureGreaterThanZero(port, "port");
        this.user = ensureNotBlank(user, "user");
        this.password = ensureNotBlank(password, "password");
        this.database = ensureNotBlank(database, "database");
        this.graph = ensureNotBlank(graphName, "graph");

        createGraph = getOrDefault(createGraph, true);
        dropGraphFirst = getOrDefault(dropGraphFirst, false);

        try (Connection connection = setupConnection()) {
            if (dropGraphFirst) {
                connection.createStatement().executeUpdate(String.format("SELECT * FROM drop_graph('s%', true)", graphName));
            }
            if (createGraph) {
                ResultSet resultSet = connection.createStatement().executeQuery(String.format("SELECT * FROM ag_graph WHERE name = '%s'", graph));
                if (!resultSet.isBeforeFirst() && resultSet.getRow() == 0) {
                    connection.createStatement().execute(String.format("SELECT * FROM ag_catalog.create_graph('%s')", graph));
                }
            }

        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    public void addVertexes(List<GraphVertex> vertexes) {
        ensureNotEmpty(vertexes, vertexes.toString());
        vertexes.stream().forEach(this::addVertex);
    }

    public boolean addVertex(GraphVertex vertex) {
        log.info("Add vertex:{}", vertex);
        ensureNotNull(vertex, vertex.toString());
        ensureNotEmpty(vertex.getMetadata(), "Metadata");
        try (Connection connection = setupConnection()) {
            String label = vertex.getLabel();
            String prepareSql = """
                    SELECT *
                    FROM cypher('%s', $$
                        create (%s {name:$name,textSegmentId:$textSegmentId,description:$description,metadata:$metadata})
                    $$, ?) as (a agtype);
                    """.formatted(graph, StringUtils.isNotBlank(label) ? ":" + label : "");
            log.info("prepareSql:{}", prepareSql);
            PreparedStatement upsertStmt = connection.prepareStatement(prepareSql);
            Agtype agtype = new Agtype();
            agtype.setValue(JsonUtil.toJson(vertex));
            upsertStmt.setObject(1, agtype);
            return upsertStmt.execute();
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Update vertex
     *
     * @param updateInfo
     * @return
     */
    public GraphVertex updateVertex(GraphVertexUpdateInfo updateInfo) {
        log.info("Update vertex:{}", updateInfo.getNewData());
        ensureNotNull(updateInfo.getMetadataFilter(), "Metadata filter");
        GraphVertex newData = updateInfo.getNewData();
        ensureNotNull(newData, newData.toString());

        try (Connection connection = setupConnection()) {

            GraphSearchCondition whereCondition = GraphSearchCondition.builder()
                    .names(List.of(updateInfo.getName()))
                    .metadataFilter(updateInfo.getMetadataFilter())
                    .build();
            String whereClause = GraphStoreUtil.buildWhereClause(whereCondition, "v", "filter");
            String setClause = GraphStoreUtil.buildSetClause(updateInfo.getNewData().getMetadata());
            String prepareSql = """
                    select * from cypher('%s', $$
                       match (v)
                       where %s
                       set v.textSegmentId=$new_textSegmentId,v.description=$new_description%s
                       return v
                       limit 1
                    $$, ?) as (v agtype);
                    """.formatted(graph, whereClause, setClause);
            log.info("prepareSql:{}", prepareSql);
            PreparedStatement stmt = connection.prepareStatement(prepareSql);

            Map<String, Object> whereArgs = GraphStoreUtil.buildWhereArgs(whereCondition, "filter");
            Map<String, Object> setArgs = GraphStoreUtil.buildSetArgs(updateInfo.getNewData().getMetadata());
            whereArgs.putAll(setArgs);
            whereArgs.putAll(Map.of("new_textSegmentId", newData.getTextSegmentId(), "new_description", newData.getDescription()));
            log.info("updateVertex args:{}", whereArgs);

            Agtype agtype = new Agtype();
            agtype.setValue(JsonUtil.toJson(whereArgs));
            stmt.setObject(1, agtype);
            stmt.execute();
            return getVertexFromResultSet(stmt.getResultSet());
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    public GraphVertex getVertex(GraphVertexSearch search) {
        List<GraphVertex> list = this.searchVertices(search);
        if (list.isEmpty()) {
            return null;
        }
        return list.get(0);
    }

    public List<GraphVertex> getVertices(List<Long> ids) {
        try (Connection connection = setupConnection()) {
            String query = """
                    select * from cypher('%s', $$
                        match (v)
                        where id(v) in [%s]
                        return v
                    $$) as (v agtype);
                    """.formatted(graph, Joiner.on(",").join(ids));
            log.info("getVertices query:{}", query);
            ResultSet resultSet = connection.createStatement().executeQuery(query);
            return getVerticesFromResultSet(resultSet);
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * @param search
     * @return
     */
    public List<GraphVertex> searchVertices(GraphVertexSearch search) {
        try (Connection connection = setupConnection()) {
            String label = search.getLabel();
            String whereClause = GraphStoreUtil.buildWhereClause(search, "v", "filter");
            String query = """
                    select * from cypher('%s', $$
                        match (%s)
                        with v
                        order by id(v) desc
                        where %s and id(v) < %d
                        return v
                        limit %d
                    $$,?) as (v agtype);
                    """.formatted(graph, StringUtils.isNotBlank(label) ? "v:" + label : "v", whereClause, search.getMaxId(), search.getLimit());
            log.info("SearchVertices prepareSql:{}", query);

            PreparedStatement selectStmt = connection.prepareStatement(query);
            Map<String, Object> whereArgs = GraphStoreUtil.buildWhereArgs(search, "filter");
            log.info("getVertex args:{}", whereArgs);

            Agtype agtype = new Agtype();
            agtype.setValue(JsonUtil.toJson(whereArgs));
            selectStmt.setObject(1, agtype);
            ResultSet resultSet = selectStmt.executeQuery();
            return getVerticesFromResultSet(resultSet);
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    public List<Triple<GraphVertex, GraphEdge, GraphVertex>> getEdges(List<Long> ids) {
        try (Connection connection = setupConnection()) {
            String query = """
                    select * from cypher('%s', $$
                        match (v1)-[e]->(v2)
                        where id(e) in [%s]
                        return v1,e,v2
                    $$) as (e agtype);
                    """.formatted(graph, Joiner.on(",").join(ids));
            log.info("getEdges query:{}", query);
            ResultSet resultSet = connection.createStatement().executeQuery(query);
            return getEdgesFromResultSet(resultSet);
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }


    public List<Triple<GraphVertex, GraphEdge, GraphVertex>> searchEdges(GraphEdgeSearch search) {
        try (Connection connection = setupConnection()) {
            String filterClause1 = GraphStoreUtil.buildWhereClause(search.getSource(), "v1", "filter1");
            String filterClause2 = GraphStoreUtil.buildWhereClause(search.getTarget(), "v2", "filter2");
            String filterClause3 = GraphStoreUtil.buildWhereClause(search.getEdge(), "e", "filter3");
            String filterClause = filterClause1;
            if (StringUtils.isNotBlank(filterClause2)) {
                filterClause += StringUtils.isNotBlank(filterClause) ? " and " + filterClause2 : filterClause2;
            }
            if (StringUtils.isNotBlank(filterClause3)) {
                filterClause += StringUtils.isNotBlank(filterClause) ? " and " + filterClause3 : filterClause3;
            }
            String query = """
                    select * from cypher('%s', $$
                        match (v1)-[e]-(v2)
                        with v1,e,v2
                        order by id(e) desc
                        where %s and id(e) < %d
                        return v1,e,v2
                        limit %d
                    $$,?) as (v1 agtype,e agtype,v2 agtype);
                    """.formatted(graph, filterClause, search.getMaxId(), search.getLimit());
            log.info("Search edges prepareSql:\n{}", query);
            PreparedStatement selectStmt = connection.prepareStatement(query);

            Map<String, Object> whereArgs1 = GraphStoreUtil.buildWhereArgs(search.getSource(), "filter1");
            Map<String, Object> whereArgs2 = GraphStoreUtil.buildWhereArgs(search.getTarget(), "filter2");
            Map<String, Object> whereArgs3 = GraphStoreUtil.buildWhereArgs(search.getEdge(), "filter3");
            whereArgs1.putAll(whereArgs2);
            whereArgs1.putAll(whereArgs3);
            Agtype agtype = new Agtype();
            agtype.setValue(JsonUtil.toJson(whereArgs1));
            selectStmt.setObject(1, agtype);
            log.info("Search edges args:{}", agtype);
            ResultSet resultSet = selectStmt.executeQuery();
            return getEdgesFromResultSet(resultSet);
        } catch (SQLException e) {
            throw new RuntimeException(e);
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
        try (Connection connection = setupConnection()) {
            String whereClause1 = GraphStoreUtil.buildWhereClause(addInfo.getSourceFilter(), "v1", "filter1");
            String whereClause2 = GraphStoreUtil.buildWhereClause(addInfo.getTargetFilter(), "v2", "filter2");
            String prepareSql = """
                    select * from cypher('%s', $$
                      match (v1), (v2)
                      where %s
                      create (v1)-[e:RELTYPE {textSegmentId:$textSegmentId,weight:$weight,description:$description,metadata:$metadata}]->(v2)
                      return v1,e,v2
                    $$, ?) as (v1 agtype,e agtype,v2 agtype);
                    """.formatted(graph, whereClause1 + " and " + whereClause2);
            log.info("Add edge prepareSql:{}", prepareSql);
            PreparedStatement preparedStatement = connection.prepareStatement(prepareSql);

            Map<String, Object> whereArgs1 = GraphStoreUtil.buildWhereArgs(addInfo.getSourceFilter(), "filter1");
            Map<String, Object> whereArgs2 = GraphStoreUtil.buildWhereArgs(addInfo.getTargetFilter(), "filter2");
            whereArgs1.putAll(whereArgs2);
            whereArgs1.putAll(JsonUtil.toMap(addInfo.getEdge()));
            Agtype agtype = new Agtype();
            agtype.setValue(JsonUtil.toJson(whereArgs1));
            preparedStatement.setObject(1, agtype);
            preparedStatement.execute();
            return getEdgeFromResultSet(preparedStatement.getResultSet());
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    public Triple<GraphVertex, GraphEdge, GraphVertex> updateEdge(GraphEdgeEditInfo edgeEditInfo) {
        log.info("Update edge:{}", edgeEditInfo);
        ensureNotNull(edgeEditInfo.getEdge(), "Graph edit info");
        GraphEdge newData = edgeEditInfo.getEdge();
        try (Connection connection = setupConnection()) {
            String whereClause1 = GraphStoreUtil.buildWhereClause(edgeEditInfo.getSourceFilter(), "v1", "filter1");
            String whereClause2 = GraphStoreUtil.buildWhereClause(edgeEditInfo.getTargetFilter(), "v2", "filter2");
            String setClause = GraphStoreUtil.buildSetClause(edgeEditInfo.getEdge().getMetadata());
            String prepareSql = """
                    select * from cypher('%s', $$
                       match (v1)-[e]->(v2)
                       where %s
                       set e.weight=$new_weight,e.textSegmentId=$new_textSegmentId,e.description=$new_description %s
                       return v1,e,v2
                    $$, ?) as (v1 agtype,e agtype,v2 agtype);
                    """.formatted(graph, whereClause1 + " and " + whereClause2, setClause);
            log.info("prepareSql:{}", prepareSql);
            PreparedStatement upsertStmt = connection.prepareStatement(prepareSql);
            Map<String, Object> whereArgs1 = GraphStoreUtil.buildWhereArgs(edgeEditInfo.getSourceFilter(), "filter1");
            Map<String, Object> whereArgs2 = GraphStoreUtil.buildWhereArgs(edgeEditInfo.getTargetFilter(), "filter2");
            Map<String, Object> setArgs = GraphStoreUtil.buildSetArgs(edgeEditInfo.getEdge().getMetadata());
            whereArgs1.putAll(whereArgs2);
            whereArgs1.putAll(setArgs);
            whereArgs1.putAll(
                    Map.of(
                            "new_textSegmentId", newData.getTextSegmentId(),
                            "new_weight", newData.getWeight(),
                            "new_description", newData.getDescription()
                    )
            );
            Agtype agtype = new Agtype();
            agtype.setValue(JsonUtil.toJson(whereArgs1));
            upsertStmt.setObject(1, agtype);
            upsertStmt.execute();
            return getEdgeFromResultSet(upsertStmt.getResultSet());
        } catch (SQLException e) {
            throw new RuntimeException(e);
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
        try (Connection connection = setupConnection()) {
            String whereClause = GraphStoreUtil.buildWhereClause(filter, "v", "filter");
            String prepareSql = """
                    select * from cypher('%s', $$
                    	match (v)
                    	where %s
                    	%s
                    $$,?) as (v agtype);
                    """.formatted(graph, whereClause, includeEdges ? "DETACH DELETE v" : "DELETE v");
            log.info("prepareSql:{}", prepareSql);
            PreparedStatement upsertStmt = connection.prepareStatement(prepareSql);
            Map<String, Object> whereArgs = GraphStoreUtil.buildWhereArgs(filter, "filter");
            Agtype agtype = new Agtype();
            agtype.setValue(JsonUtil.toJson(whereArgs));
            upsertStmt.setObject(1, agtype);
            upsertStmt.execute();
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * 单独删除边
     *
     * @param filter
     */
    public void deleteEdges(GraphSearchCondition filter) {
        ensureNotNull(filter, "Data filter");
        try (Connection connection = setupConnection()) {
            String whereClause = GraphStoreUtil.buildWhereClause(filter, "r", "filter");
            String prepareSql = """
                    select * from cypher('%s', $$
                    	match ()-[r]->()
                    	where %s
                    	delete r
                    $$,?) as (r agtype);
                    """.formatted(graph, whereClause);
            log.info("prepareSql:{}", prepareSql);
            PreparedStatement upsertStmt = connection.prepareStatement(prepareSql);
            Map<String, Object> whereArgs = GraphStoreUtil.buildWhereArgs(filter, "filter");
            Agtype agtype = new Agtype();
            agtype.setValue(JsonUtil.toJson(whereArgs));
            upsertStmt.setObject(1, agtype);
            upsertStmt.execute();
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    private List<Triple<GraphVertex, GraphEdge, GraphVertex>> getEdgesFromResultSet(ResultSet resultSet) {
        List<Triple<GraphVertex, GraphEdge, GraphVertex>> result = new ArrayList<>();
        try {
            while (resultSet.next()) {
                Agtype source = resultSet.getObject(1, Agtype.class);
                Agtype edge = resultSet.getObject(2, Agtype.class);
                Agtype target = resultSet.getObject(3, Agtype.class);
                result.add(Triple.of(agTypeToVertex(source), agTypeToEdge(edge), agTypeToVertex(target)));
            }
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
        return result;
    }

    public Triple<GraphVertex, GraphEdge, GraphVertex> getEdgeFromResultSet(ResultSet resultSet) {
        List<Triple<GraphVertex, GraphEdge, GraphVertex>> list = getEdgesFromResultSet(resultSet);
        if (list.isEmpty()) {
            return null;
        }
        return list.get(0);
    }

    public GraphVertex getVertexFromResultSet(ResultSet resultSet) {
        List<GraphVertex> vertices = getVerticesFromResultSet(resultSet);
        if (vertices.isEmpty()) {
            return null;
        }
        return vertices.get(0);
    }

    public List<GraphVertex> getVerticesFromResultSet(ResultSet resultSet) {
        List<GraphVertex> vertices = new ArrayList<>();
        try {
            while (resultSet.next()) {
                Agtype returnedAgtype = resultSet.getObject(1, Agtype.class);
                vertices.add(agTypeToVertex(returnedAgtype));
            }
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
        return vertices;
    }

    public GraphVertex agTypeToVertex(Agtype agtype) {
        AgtypeMap agtypeMap = agtype.getMap();
        Long id = agtypeMap.getLong("id");
        String label = agtypeMap.getObject("label").toString();
        AgtypeMap nodeProps = agtypeMap.getMap("properties");
        Map<String, Object> map = new HashMap<>();
        for (Map.Entry<String, Object> entry : nodeProps.getMap("metadata").entrySet()) {
            map.put(entry.getKey(), entry.getValue());
        }
        return GraphVertex.builder()
                .id(id)
                .label(label)
                .name(nodeProps.getString("name"))
                .description(nodeProps.getString("description"))
                .textSegmentId(nodeProps.getString("textSegmentId"))
                .metadata(map)
                .build();
    }

    private GraphEdge agTypeToEdge(Agtype agtype) {
        AgtypeMap agtypeMap = agtype.getMap();
        Long id = agtypeMap.getLong("id");
        Long startId = agtypeMap.getLong("start_id");
        Long endId = agtypeMap.getLong("end_id");
        String nodeLabel = agtypeMap.getObject("label").toString();
        AgtypeMap nodeProps = agtypeMap.getMap("properties");
        Map<String, Object> map = new HashMap<>();
        for (Map.Entry<String, Object> entry : nodeProps.getMap("metadata").entrySet()) {
            map.put(entry.getKey(), entry.getValue());
        }
        return GraphEdge.builder()
                .id(id)
                .startId(startId)
                .endId(endId)
                .label(nodeLabel)
                .weight(null == nodeProps.getObject("weight") ? 0 : nodeProps.getDouble("weight"))
                .description(nodeProps.getString("description"))
                .textSegmentId(nodeProps.getString("textSegmentId"))
                .metadata(map)
                .build();
    }

    private Connection setupConnection() throws SQLException {
        PgConnection connection = DriverManager.getConnection(
                String.format("jdbc:postgresql://%s:%s/%s", host, port, database),
                user,
                password
        ).unwrap(PgConnection.class);
        connection.addDataType("agtype", Agtype.class);
        Statement stmt = connection.createStatement();
//        stmt.execute("CREATE EXTENSION IF NOT EXISTS age;");
        stmt.execute("LOAD 'age'");
        stmt.execute("SET search_path = ag_catalog, \"$user\", public;");
        return connection;
    }
}
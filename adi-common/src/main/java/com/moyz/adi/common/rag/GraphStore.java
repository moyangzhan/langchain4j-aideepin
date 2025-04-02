package com.moyz.adi.common.rag;

import com.moyz.adi.common.vo.*;
import org.apache.commons.lang3.tuple.Triple;

import java.util.List;

public interface GraphStore {
    boolean addVertexes(List<GraphVertex> vertexes);

    boolean addVertex(GraphVertex vertex);

    GraphVertex updateVertex(GraphVertexUpdateInfo updateInfo);

    GraphVertex getVertex(GraphVertexSearch search);

    List<GraphVertex> getVertices(List<String> ids);

    List<GraphVertex> searchVertices(GraphVertexSearch search);

    List<Triple<GraphVertex, GraphEdge, GraphVertex>> getEdges(List<String> ids);

    List<Triple<GraphVertex, GraphEdge, GraphVertex>> searchEdges(GraphEdgeSearch search);

    Triple<GraphVertex, GraphEdge, GraphVertex> getEdge(GraphEdgeSearch search);

    Triple<GraphVertex, GraphEdge, GraphVertex> addEdge(GraphEdgeAddInfo addInfo);

    Triple<GraphVertex, GraphEdge, GraphVertex> updateEdge(GraphEdgeEditInfo edgeEditInfo);

    void deleteVertices(GraphSearchCondition filter, boolean includeEdges);

    void deleteEdges(GraphSearchCondition filter);
}

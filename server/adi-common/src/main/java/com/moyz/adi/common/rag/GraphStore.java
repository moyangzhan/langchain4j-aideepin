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

    /**
     * 定点删除源/目标实体名与 kb 匹配的边（段停用时按账本清理独占边）。
     * <p>
     * 无向匹配：与 searchEdges/getEdge 的查找语义保持一致——方向不参与边的身份，
     * 账本中按字典序规范化记账的边与其在图库中的建边方向可能相反。
     */
    void deleteEdge(String kbUuid, String sourceName, String targetName);
}

package com.moyz.adi.common.service;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.dto.GraphContributionDto;
import com.moyz.adi.common.rag.GraphStore;
import com.moyz.adi.common.vo.*;
import dev.langchain4j.store.embedding.filter.Filter;
import dev.langchain4j.store.embedding.filter.comparison.IsEqualTo;
import dev.langchain4j.store.embedding.filter.comparison.IsIn;
import jakarta.annotation.Resource;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.commons.lang3.tuple.Triple;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

@Service
public class KnowledgeBaseGraphService {

    @Resource
    private GraphStore kbGraphStore;

    @Resource
    private DocumentGraphProvenanceService documentGraphProvenanceService;

    public List<GraphVertex> listVerticesByKbUuid(String kbUuid, long maxId, int limit) {
        Filter filter = new IsEqualTo(AdiConstant.MetadataKey.KB_UUID, kbUuid);
        return kbGraphStore.searchVertices(GraphVertexSearch.builder()
                .metadataFilter(filter)
                .maxId(maxId)
                .limit(limit)
                .build());
    }

    public List<GraphVertex> listVerticesByKbItemUuid(String kbItemUuid, long maxId, int limit) {
        Filter filter = new IsEqualTo(AdiConstant.MetadataKey.KB_ITEM_UUID, kbItemUuid);
        return kbGraphStore.searchVertices(
                GraphVertexSearch.builder()
                        .limit(limit)
                        .maxId(maxId)
                        .metadataFilter(filter)
                        .build()
        );
    }

    public List<Triple<GraphVertex, GraphEdge, GraphVertex>> listEdgesByKbUuid(String kbUuid, long maxId, int limit) {
        Filter filter = new IsEqualTo(AdiConstant.MetadataKey.KB_UUID, kbUuid);
        return kbGraphStore.searchEdges(GraphEdgeSearch.builder()
                .edge(GraphSearchCondition.builder().metadataFilter(filter).build())
                .maxId(maxId)
                .limit(limit)
                .build());
    }

    public List<Triple<GraphVertex, GraphEdge, GraphVertex>> listEdgesByKbItemUuid(String kbItemUuid, long maxId, int limit) {
        Filter filter = new IsEqualTo(AdiConstant.MetadataKey.KB_ITEM_UUID, kbItemUuid);
        return kbGraphStore.searchEdges(GraphEdgeSearch.builder()
                .edge(GraphSearchCondition.builder().metadataFilter(filter).build())
                .maxId(maxId)
                .limit(limit)
                .build());
    }

    public Pair<List<GraphVertex>, List<GraphEdge>> getFromTriple(List<Triple<GraphVertex, GraphEdge, GraphVertex>> triples) {
        List<GraphVertex> vertices = new ArrayList<>();
        List<GraphEdge> edges = new ArrayList<>();
        for (Triple<GraphVertex, GraphEdge, GraphVertex> triple : triples) {
            vertices.add(triple.getLeft());
            vertices.add(triple.getRight());
            edges.add(triple.getMiddle());
        }
        return Pair.of(vertices, edges);
    }

    /**
     * 清理某段在图谱中的足迹（账本驱动，幂等）：独占边/顶点从图库删除，共享元素保留，
     * 最后删该段全部账本行。段停用流程与启用前的残留清理复用。
     * <p>
     * 先删独占边、再删独占顶点：DETACH DELETE 会级联删边，配合"边的每个贡献者必为
     * 两端顶点贡献者"的双写不变式，此顺序保证不误删他段数据。
     */
    public void removeSegmentGraphFootprint(String kbUuid, String segmentUuid) {
        List<GraphContributionDto> edges = documentGraphProvenanceService.listEdgesWithExclusivity(segmentUuid);
        for (GraphContributionDto edge : edges) {
            if (edge.getOtherContributors() != null && edge.getOtherContributors() == 0) {
                kbGraphStore.deleteEdge(kbUuid, edge.getName(), edge.getTargetName());
            }
        }
        List<GraphContributionDto> vertices = documentGraphProvenanceService.listVerticesWithExclusivity(segmentUuid);
        for (GraphContributionDto vertex : vertices) {
            if (vertex.getOtherContributors() != null && vertex.getOtherContributors() == 0) {
                kbGraphStore.deleteVertices(
                        GraphSearchCondition.builder()
                                .names(List.of(vertex.getName()))
                                .metadataFilter(new IsEqualTo(AdiConstant.MetadataKey.KB_UUID, kbUuid))
                                .build(),
                        true);
            }
        }
        documentGraphProvenanceService.deleteBySegmentUuid(segmentUuid);
    }

    /**
     * 清理某文档在图谱中的足迹（账本驱动，幂等）：文档重跑图谱的前置清理、文档删除、
     * 漂移修复共用。独占口径比段级宽一档——"文档外无其他贡献者"即删，本档内多段共享
     * 的元素随重抽重建，不误删他档数据。
     */
    public void removeDocumentGraphFootprint(String kbUuid, String docUuid) {
        List<GraphContributionDto> edges = documentGraphProvenanceService.listEdgesWithOutsideContributors(docUuid);
        for (GraphContributionDto edge : edges) {
            if (edge.getOtherContributors() != null && edge.getOtherContributors() == 0) {
                kbGraphStore.deleteEdge(kbUuid, edge.getName(), edge.getTargetName());
            }
        }
        List<GraphContributionDto> vertices = documentGraphProvenanceService.listVerticesWithOutsideContributors(docUuid);
        for (GraphContributionDto vertex : vertices) {
            if (vertex.getOtherContributors() != null && vertex.getOtherContributors() == 0) {
                kbGraphStore.deleteVertices(
                        GraphSearchCondition.builder()
                                .names(List.of(vertex.getName()))
                                .metadataFilter(new IsEqualTo(AdiConstant.MetadataKey.KB_UUID, kbUuid))
                                .build(),
                        true);
            }
        }
        documentGraphProvenanceService.deleteByDocUuid(docUuid);
    }
}

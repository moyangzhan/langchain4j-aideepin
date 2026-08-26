package com.moyz.adi.common.service;

import com.moyz.adi.common.dto.GraphContributionDto;
import com.moyz.adi.common.dto.KbEdgeDto;
import com.moyz.adi.common.dto.KbVertexDto;
import com.moyz.adi.common.entity.DocumentGraphEdge;
import com.moyz.adi.common.entity.DocumentGraphVertex;
import com.moyz.adi.common.mapper.DocumentGraphEdgeMapper;
import com.moyz.adi.common.mapper.DocumentGraphVertexMapper;
import jakarta.annotation.Resource;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * 图谱段溯源账本的统一门面（跨 adi_document_graph_vertex / _edge 两表）。
 * <p>
 * 账本是"图谱元素 ↔ 分段"多对多贡献关系的唯一事实源：段级/文档级图谱清理的圈定与
 * 独占判定、文档图谱页的聚合展示都从这里取数；图库上的合并描述只是检索缓存。
 * 漂移裁决原则：账本为唯一标准，图库数据不作为任何决策依据（见设计文档 2.1）。
 */
@Service
public class DocumentGraphProvenanceService {

    @Resource
    private DocumentGraphVertexMapper vertexMapper;

    @Resource
    private DocumentGraphEdgeMapper edgeMapper;

    /**
     * 批量落库贡献行（ON CONFLICT DO NOTHING，重复抽取天然幂等）
     */
    public void saveContributions(List<DocumentGraphVertex> vertices, List<DocumentGraphEdge> edges) {
        if (CollectionUtils.isNotEmpty(vertices)) {
            vertexMapper.insertIgnoreBatch(vertices);
        }
        if (CollectionUtils.isNotEmpty(edges)) {
            edgeMapper.insertIgnoreBatch(edges);
        }
    }

    /**
     * 段停用清理用：该段贡献的顶点 + 各自的其他贡献者数（0=独占）
     */
    public List<GraphContributionDto> listVerticesWithExclusivity(String segmentUuid) {
        return vertexMapper.listWithExclusivity(segmentUuid);
    }

    /**
     * 段停用清理用：该段贡献的边 + 各自的其他贡献者数（0=独占）
     */
    public List<GraphContributionDto> listEdgesWithExclusivity(String segmentUuid) {
        return edgeMapper.listWithExclusivity(segmentUuid);
    }

    /**
     * 文档级清理用（删文档/文档重跑图谱前置清理）：该文档贡献的顶点 + 文档外贡献者数
     */
    public List<GraphContributionDto> listVerticesWithOutsideContributors(String docUuid) {
        return vertexMapper.listWithOutsideContributors(docUuid);
    }

    public List<GraphContributionDto> listEdgesWithOutsideContributors(String docUuid) {
        return edgeMapper.listWithOutsideContributors(docUuid);
    }

    /**
     * 删除该段的全部账本行（共享元素只删本段行，图上元素保留）
     */
    public void deleteBySegmentUuid(String segmentUuid) {
        vertexMapper.deleteBySegmentUuid(segmentUuid);
        edgeMapper.deleteBySegmentUuid(segmentUuid);
    }

    /**
     * 删除该文档的全部账本行
     */
    public void deleteByDocUuid(String docUuid) {
        vertexMapper.deleteByDocUuid(docUuid);
        edgeMapper.deleteByDocUuid(docUuid);
    }

    /**
     * Detail-page aggregation: vertices (entity name + descriptions concatenated from all contributing segments);
     * a non-empty afterName loads incrementally in name order
     */
    public List<KbVertexDto> aggregateVerticesByDoc(String docUuid, String afterName, int limit) {
        return vertexMapper.aggregateByDoc(docUuid, afterName, limit);
    }

    /**
     * Detail-page aggregation: edges (normalized endpoint pair + concatenated description + summed weight);
     * non-empty afterSource/afterTarget loads incrementally
     */
    public List<KbEdgeDto> aggregateEdgesByDoc(String docUuid, String afterSource, String afterTarget, int limit) {
        return edgeMapper.aggregateByDoc(docUuid, afterSource, afterTarget, limit);
    }

    /**
     * Distinct element counts of the document graph (pagination progress display)
     */
    public long countVerticesByDoc(String docUuid) {
        return vertexMapper.countDistinctByDoc(docUuid);
    }

    public long countEdgesByDoc(String docUuid) {
        return edgeMapper.countDistinctByDoc(docUuid);
    }
}

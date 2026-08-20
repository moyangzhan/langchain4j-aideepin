package com.moyz.adi.chat.controller;

import com.moyz.adi.common.dto.KbEdgeDto;
import com.moyz.adi.common.dto.KbVertexDto;
import com.moyz.adi.common.service.DocumentGraphProvenanceService;
import com.moyz.adi.common.service.KnowledgeBaseGraphService;
import com.moyz.adi.common.service.KbDocumentService;
import com.moyz.adi.common.vo.GraphEdge;
import com.moyz.adi.common.vo.GraphVertex;
import jakarta.annotation.Resource;
import org.apache.commons.lang3.tuple.Triple;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

@RestController
@RequestMapping("/knowledge-base-graph")
@Validated
public class KnowledgeBaseGraphController {

    /**
     * 图谱页默认聚合上限（limit 未指定时）
     */
    private static final int DEFAULT_AGGREGATE_LIMIT = 200;

    @Resource
    private KnowledgeBaseGraphService knowledgeBaseGraphService;

    @Resource
    private KbDocumentService kbDocumentService;

    @Resource
    private DocumentGraphProvenanceService documentGraphProvenanceService;

    /**
     * 文档图谱（纯账本聚合）：顶点=实体名+片段拼接描述，边=规范化端点对+拼接描述+强度求和。
     * 响应以实体名为键、不含图库内部 id，渲染与展示对图库零依赖；
     * 停用段的贡献行已删——独占元素消失、共享元素聚合描述正确收缩。
     * 存量文档（账本无行，升级前图谱化）回退图库查询并映射为同一返回结构（懒迁移语义）。
     */
    @GetMapping("/list/{kbItemUuid}")
    public Map<String, Object> list(@PathVariable String kbItemUuid, @RequestParam(defaultValue = Long.MAX_VALUE + "") Long maxVertexId, @RequestParam(defaultValue = Long.MAX_VALUE + "") Long maxEdgeId, @RequestParam(defaultValue = "-1") int limit) {
        kbDocumentService.checkReadPrivilege(kbItemUuid);
        if (documentGraphProvenanceService.existsByDocUuid(kbItemUuid)) {
            int cap = limit > 0 ? limit : DEFAULT_AGGREGATE_LIMIT;
            return Map.of(
                    "vertices", documentGraphProvenanceService.aggregateVerticesByDoc(kbItemUuid, cap),
                    "edges", documentGraphProvenanceService.aggregateEdgesByDoc(kbItemUuid, cap));
        }
        return legacyFromGraphStore(kbItemUuid, maxVertexId, maxEdgeId, limit);
    }

    /**
     * 存量兜底：按图库查询后映射为与账本聚合一致的返回结构（顶点按实体名去重）
     */
    private Map<String, Object> legacyFromGraphStore(String kbItemUuid, Long maxVertexId, Long maxEdgeId, int limit) {
        List<GraphVertex> vertices = knowledgeBaseGraphService.listVerticesByKbItemUuid(kbItemUuid, maxVertexId, limit);
        List<Triple<GraphVertex, GraphEdge, GraphVertex>> edgeWithVertices = knowledgeBaseGraphService.listEdgesByKbItemUuid(kbItemUuid, maxEdgeId, limit);
        List<KbVertexDto> vertexDtos = new ArrayList<>(vertices.stream()
                .collect(Collectors.toMap(GraphVertex::getName, Function.identity(), (s, a) -> s))
                .values()
                .stream()
                .map(v -> {
                    KbVertexDto dto = new KbVertexDto();
                    dto.setName(v.getName());
                    dto.setDescription(v.getDescription());
                    return dto;
                })
                .toList());
        List<KbEdgeDto> edgeDtos = edgeWithVertices.stream()
                .map(t -> {
                    KbEdgeDto dto = new KbEdgeDto();
                    dto.setSourceName(t.getLeft().getName());
                    dto.setTargetName(t.getRight().getName());
                    dto.setDescription(t.getMiddle().getDescription());
                    dto.setWeight(t.getMiddle().getWeight());
                    return dto;
                })
                .toList();
        return Map.of("vertices", vertexDtos, "edges", edgeDtos);
    }
}

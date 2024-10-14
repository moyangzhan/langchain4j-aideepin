package com.moyz.adi.chat.controller;

import com.moyz.adi.common.service.KnowledgeBaseGraphService;
import com.moyz.adi.common.vo.GraphEdge;
import com.moyz.adi.common.vo.GraphVertex;
import jakarta.annotation.Resource;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.commons.lang3.tuple.Triple;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

@RestController
@RequestMapping("/knowledge-base-graph")
@Validated
public class KnowledgeBaseGraphController {
    @Resource
    private KnowledgeBaseGraphService knowledgeBaseGraphService;

    @GetMapping("/list/{kbItemUuid}")
    public Map<String, Object> list(@PathVariable String kbItemUuid, @RequestParam(defaultValue = Long.MAX_VALUE + "") Long maxVertexId, @RequestParam(defaultValue = Long.MAX_VALUE + "") Long maxEdgeId, @RequestParam(defaultValue = "-1") int limit) {
        List<GraphVertex> vertices = knowledgeBaseGraphService.listVerticesByKbItemUuid(kbItemUuid, maxVertexId, limit);
        List<Triple<GraphVertex, GraphEdge, GraphVertex>> edgeWithVertices = knowledgeBaseGraphService.listEdgesByKbItemUuid(kbItemUuid, maxEdgeId, limit);
        Pair<List<GraphVertex>, List<GraphEdge>> pair = knowledgeBaseGraphService.getFromTriple(edgeWithVertices);
        vertices.addAll(pair.getLeft());
        //去重
        List<GraphVertex> filteredVertices = vertices
                .stream()
                .collect(
                        Collectors.toMap(
                                GraphVertex::getId, Function.identity(), (s, a) -> s
                        )
                )
                .values()
                .stream()
                .toList();
        return Map.of("vertices", filteredVertices, "edges", pair.getRight());
    }
}

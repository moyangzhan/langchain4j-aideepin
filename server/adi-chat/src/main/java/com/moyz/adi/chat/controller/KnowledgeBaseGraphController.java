package com.moyz.adi.chat.controller;

import com.moyz.adi.common.service.DocumentGraphProvenanceService;
import com.moyz.adi.common.service.KbDocumentService;
import jakarta.annotation.Resource;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

@RestController
@RequestMapping("/knowledge-base-graph")
@Validated
public class KnowledgeBaseGraphController {

    /**
     * Default aggregate limit when the caller does not specify one
     */
    private static final int DEFAULT_AGGREGATE_LIMIT = 200;

    @Resource
    private KbDocumentService kbDocumentService;

    @Resource
    private DocumentGraphProvenanceService documentGraphProvenanceService;

    /**
     * Document graph from the provenance ledger, name-ordered cursor pagination: vertices are
     * entity names with descriptions concatenated from contributing segments, edges are
     * normalized endpoint pairs with summed weights. Keys are entity names (no graph-store ids).
     * The first page carries no cursor; follow-ups pass the last element of the previous batch
     * (vertex name / edge endpoint pair).
     */
    @GetMapping("/list/{kbItemUuid}")
    public Map<String, Object> list(@PathVariable String kbItemUuid,
                                    @RequestParam(required = false) String afterVertex,
                                    @RequestParam(required = false) String afterEdgeSource,
                                    @RequestParam(required = false) String afterEdgeTarget,
                                    @RequestParam(defaultValue = "-1") int limit) {
        kbDocumentService.checkReadPrivilege(kbItemUuid);
        int cap = limit > 0 ? limit : DEFAULT_AGGREGATE_LIMIT;
        return Map.of(
                "vertices", documentGraphProvenanceService.aggregateVerticesByDoc(kbItemUuid, afterVertex, cap),
                "edges", documentGraphProvenanceService.aggregateEdgesByDoc(kbItemUuid, afterEdgeSource, afterEdgeTarget, cap),
                "totalVertices", documentGraphProvenanceService.countVerticesByDoc(kbItemUuid),
                "totalEdges", documentGraphProvenanceService.countEdgesByDoc(kbItemUuid));
    }
}

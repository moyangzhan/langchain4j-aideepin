package com.moyz.adi.common.dto;

import com.moyz.adi.common.vo.GraphEdge;
import com.moyz.adi.common.vo.GraphVertex;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * RefGraphDto.resolveEdgeNames 契约钉子：边端点按顶点 id 反查实体名、重复边按 id 去重、悬空边丢弃、无 id 边不去重。
 */
class RefGraphDtoTest {

    private static GraphVertex vertex(String id, String name) {
        return GraphVertex.builder().id(id).name(name).build();
    }

    private static GraphEdge edge(String id, String startId, String endId) {
        return GraphEdge.builder().id(id).startId(startId).endId(endId).build();
    }

    @Test
    void resolvesEndpointNamesByVertexIdAndDeduplicatesById() {
        RefGraphDto dto = RefGraphDto.builder()
                .vertices(List.of(vertex("v1", "IT部门"), vertex("v2", "协调会议")))
                .edges(List.of(edge("e1", "v1", "v2"), edge("e1", "v1", "v2"), edge("e2", "v2", "v1")))
                .build();

        dto.resolveEdgeNames();

        assertEquals(2, dto.getEdges().size());
        assertEquals("IT部门", dto.getEdges().get(0).getSourceName());
        assertEquals("协调会议", dto.getEdges().get(0).getTargetName());
        assertEquals("协调会议", dto.getEdges().get(1).getSourceName());
        assertEquals("IT部门", dto.getEdges().get(1).getTargetName());
    }

    @Test
    void dropsEdgesWithoutMatchingVertices() {
        RefGraphDto dto = RefGraphDto.builder()
                .vertices(List.of(vertex("v1", "IT部门")))
                .edges(List.of(edge("e1", "v1", "missing"), edge("e2", "missing", "v1"), edge("e3", "v1", "v1")))
                .build();

        dto.resolveEdgeNames();

        assertEquals(1, dto.getEdges().size());
        assertEquals("e3", dto.getEdges().get(0).getId());
    }

    @Test
    void keepsBlankIdEdgesWithoutDeduplication() {
        RefGraphDto dto = RefGraphDto.builder()
                .vertices(List.of(vertex("v1", "IT部门"), vertex("v2", "协调会议")))
                .edges(List.of(edge("", "v1", "v2"), edge("", "v1", "v2")))
                .build();

        dto.resolveEdgeNames();

        assertEquals(2, dto.getEdges().size());
        assertTrue(dto.getEdges().stream().allMatch(e -> "IT部门".equals(e.getSourceName())));
    }

    @Test
    void sameIdDifferentNameVerticesResolveToFirstVertex() {
        RefGraphDto dto = RefGraphDto.builder()
                .vertices(List.of(vertex("v1", "IT部门"), vertex("v1", "重复实体")))
                .edges(List.of(edge("e1", "v1", "v1")))
                .build();

        dto.resolveEdgeNames();

        assertEquals("IT部门", dto.getEdges().get(0).getSourceName());
        assertEquals("IT部门", dto.getEdges().get(0).getTargetName());
    }
}

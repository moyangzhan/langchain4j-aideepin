package com.moyz.adi.common.dto;

import com.moyz.adi.common.vo.GraphEdge;
import com.moyz.adi.common.vo.GraphVertex;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

@Builder
@Data
@NoArgsConstructor
@AllArgsConstructor
public class RefGraphDto {
    private List<String> entitiesFromQuestion;
    private List<GraphVertex> vertices;
    private List<GraphEdge> edges;

    /**
     * 将边的 startId/endId 按顶点 id 反查为 sourceName/targetName；重复边按 id 去重，端点无对应顶点的边丢弃。
     * <p>
     * Resolve edge startId/endId into sourceName/targetName via the vertex id-to-name map;
     * duplicate edges collapse by id, and edges whose endpoints have no matching vertex are dropped.
     */
    public void resolveEdgeNames() {
        Map<String, String> nameById = vertices.stream()
                .filter(item -> StringUtils.isNotBlank(item.getId()) && StringUtils.isNotBlank(item.getName()))
                .collect(Collectors.toMap(GraphVertex::getId, GraphVertex::getName, (a, b) -> a));
        List<GraphEdge> resolved = new ArrayList<>();
        Set<String> seenIds = new HashSet<>();
        for (GraphEdge edge : edges) {
            if (StringUtils.isNotBlank(edge.getId()) && !seenIds.add(edge.getId())) {
                continue;
            }
            String sourceName = nameById.get(edge.getStartId());
            String targetName = nameById.get(edge.getEndId());
            if (null == sourceName || null == targetName) {
                continue;
            }
            edge.setSourceName(sourceName);
            edge.setTargetName(targetName);
            resolved.add(edge);
        }
        edges = resolved;
    }
}

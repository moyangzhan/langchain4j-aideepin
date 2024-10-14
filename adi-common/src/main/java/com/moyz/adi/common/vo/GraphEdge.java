package com.moyz.adi.common.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Map;

@NoArgsConstructor
@AllArgsConstructor
@Builder
@Data
public class GraphEdge {
    private Long id;
    private String label;
    private Double weight;
    private String description;
    private String textSegmentId;
    private Map<String, Object> metadata;

    //Source vertex
    private Long startId;
    private String sourceName;
    private Map<String, Object> sourceMetadata;

    //Target vertex
    private Long endId;
    private String targetName;
    private Map<String, Object> targetMetadata;
}

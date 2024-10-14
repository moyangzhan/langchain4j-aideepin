package com.moyz.adi.common.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Map;

@Builder
@Data
@AllArgsConstructor
@NoArgsConstructor
public class GraphEdgeGet {
    private String source;
    private String target;
    private Map<String, Object> sourceMetadata;
    private Map<String, Object> targetMetadata;

}

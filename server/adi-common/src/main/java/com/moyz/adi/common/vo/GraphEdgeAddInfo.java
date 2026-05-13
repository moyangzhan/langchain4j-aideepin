package com.moyz.adi.common.vo;

import lombok.Data;

@Data
public class GraphEdgeAddInfo {
    private GraphEdge edge;

    private GraphSearchCondition sourceFilter;
    private GraphSearchCondition targetFilter;
}

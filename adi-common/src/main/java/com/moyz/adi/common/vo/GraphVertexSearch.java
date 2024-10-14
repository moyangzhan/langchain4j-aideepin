package com.moyz.adi.common.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

@SuperBuilder
@Data
@AllArgsConstructor
@NoArgsConstructor
public class GraphVertexSearch extends GraphSearchCondition {
    private String label;
    private String textSegmentId;

    @Builder.Default
    private Integer limit = 10;

    @Builder.Default
    private Long maxId = Long.MAX_VALUE;
}

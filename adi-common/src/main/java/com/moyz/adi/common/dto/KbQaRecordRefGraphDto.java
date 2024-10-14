package com.moyz.adi.common.dto;

import com.moyz.adi.common.vo.GraphEdge;
import com.moyz.adi.common.vo.GraphVertex;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Builder
@Data
@NoArgsConstructor
@AllArgsConstructor
public class KbQaRecordRefGraphDto {
    private List<String> entitiesFromLlm;
    private List<GraphVertex> vertices;
    private List<GraphEdge> edges;
}

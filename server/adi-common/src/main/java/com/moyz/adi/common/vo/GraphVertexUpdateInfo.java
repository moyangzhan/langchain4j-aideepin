package com.moyz.adi.common.vo;

import dev.langchain4j.store.embedding.filter.Filter;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
@Builder
@Data
public class GraphVertexUpdateInfo {
    private GraphVertex newData;

    //Filter
    private String name;
    private Filter metadataFilter;
}

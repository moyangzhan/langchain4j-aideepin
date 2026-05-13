package com.moyz.adi.common.workflow.node.google;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

@Data
public class GoogleNodeConfig {
    private String query;
    private String country;
    private String language;
    @JsonProperty("top_n")
    private Integer topN;
}

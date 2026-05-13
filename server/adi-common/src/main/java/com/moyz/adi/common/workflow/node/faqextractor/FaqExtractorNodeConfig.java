package com.moyz.adi.common.workflow.node.faqextractor;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;

@EqualsAndHashCode
@Data
public class FaqExtractorNodeConfig {
    @JsonProperty("top_n")
    private Integer topN;
    @JsonProperty("model_name")
    private String modelName;
    @JsonProperty("model_platform")
    private String modelPlatform;
}

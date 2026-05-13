package com.moyz.adi.common.workflow.node.tongyiwanx;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

@Data
public class TongyiwanxNodeConfig {
    @JsonProperty("model_name")
    private String modelName;
    private String prompt;
    private String size;
    private Integer seed;
}

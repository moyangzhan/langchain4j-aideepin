package com.moyz.adi.common.workflow.node.answer;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;

@EqualsAndHashCode
@Data
public class LLMAnswerNodeConfig {
    private String prompt;
    @JsonProperty("model_name")
    private String modelName;
    private Boolean streaming;
}

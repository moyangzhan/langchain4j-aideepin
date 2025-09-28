package com.moyz.adi.common.workflow.node.answer;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;
import lombok.EqualsAndHashCode;

@EqualsAndHashCode
@Data
public class LLMAnswerNodeConfig {
    @NotBlank
    private String prompt;
    @NotNull
    @JsonProperty("model_name")
    private String modelName;
    @JsonProperty("model_platform")
    private String modelPlatform;
    private Boolean streaming;
}

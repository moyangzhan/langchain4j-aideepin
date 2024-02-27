package com.moyz.adi.common.vo;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.moyz.adi.common.interfaces.AbstractLLMService;
import lombok.Data;

@Data
public class LLMModelInfo extends ModelInfo {

    @JsonIgnore
    private AbstractLLMService llmService;
}

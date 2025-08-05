package com.moyz.adi.common.vo;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.moyz.adi.common.service.languagemodel.AbstractLLMService;
import lombok.Data;

@Data
public class LLMModelInfo extends ModelInfo {

    @JsonIgnore
    private AbstractLLMService<?> llmService;
}

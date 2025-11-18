package com.moyz.adi.common.languagemodel.data;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.moyz.adi.common.languagemodel.AbstractLLMService;
import lombok.Data;

@Data
public class LLMModelInfo extends ModelInfo {

    @JsonIgnore
    private AbstractLLMService llmService;
}

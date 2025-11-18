package com.moyz.adi.common.languagemodel.data;

import com.fasterxml.jackson.databind.node.ObjectNode;
import lombok.Data;

@Data
public class ModelInfo {
    private Long modelId;
    private String modelName;
    private String modelTitle;
    private Boolean enable;
    private String modelPlatform;
    private String inputTypes;
    private Boolean isFree;
    private Boolean isReasoner;
    private Boolean isThinkingClosable;
    private ObjectNode properties;
}

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
    private String responseFormatTypes;
    private Boolean isFree;
    private Boolean isReasoner;
    private Boolean isThinkingClosable;
    private Boolean isSupportWebSearch;
    private ObjectNode properties;
}

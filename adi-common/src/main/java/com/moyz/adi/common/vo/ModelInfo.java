package com.moyz.adi.common.vo;

import lombok.Data;

@Data
public class ModelInfo {
    private Long modelId;
    private String modelName;
    private Boolean enable;
    private String modelPlatform;
    private String inputTypes;
}

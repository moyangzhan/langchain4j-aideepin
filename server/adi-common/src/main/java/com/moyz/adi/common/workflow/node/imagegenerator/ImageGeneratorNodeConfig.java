package com.moyz.adi.common.workflow.node.imagegenerator;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

@Data
public class ImageGeneratorNodeConfig {
    @JsonProperty("model_name")
    private String modelName;
    private String prompt;
    private String size;
    private Integer seed;
    /**
     * 质量档位,仅声明了 qualities 的模型(如 gpt-image-2)使用
     * <p>
     * Quality option, used only by models declaring qualities (e.g. gpt-image-2)
     */
    private String quality;
}

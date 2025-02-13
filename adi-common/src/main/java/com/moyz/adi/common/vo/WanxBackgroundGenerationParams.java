package com.moyz.adi.common.vo;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

@Data
public class WanxBackgroundGenerationParams {
    @JsonProperty("base_image_url")
    private String baseImageUrl;
    @JsonProperty("ref_image_url")
    private String refImageUrl;
    @JsonProperty("ref_prompt")
    private String refPrompt;
}

package com.moyz.adi.common.vo;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

@Data
public class OllamaSetting extends CommonAiPlatformSetting {

    @JsonProperty("base_url")
    private String baseUrl;
}

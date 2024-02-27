package com.moyz.adi.common.vo;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

@Data
public class OpenAiSetting {

    @JsonProperty("secret_key")
    private String secretKey;
}

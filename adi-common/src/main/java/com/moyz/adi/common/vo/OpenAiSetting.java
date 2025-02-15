package com.moyz.adi.common.vo;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;

@EqualsAndHashCode(callSuper = true)
@Data
public class OpenAiSetting extends CommonAiPlatformSetting {

    @JsonProperty("secret_key")
    private String secretKey;
}

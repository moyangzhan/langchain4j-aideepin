package com.moyz.adi.common.vo;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;

@EqualsAndHashCode(callSuper = true)
@Data
public class DeepSeekSetting extends CommonAiPlatformSetting {
    @JsonProperty("secret_key")
    private String secretKey;

    /**
     * <a href="https://api-docs.deepseek.com/zh-cn/">api文档</a><br/>
     * 默认：https://api.deepseek.com 或 https://api.deepseek.com/v1<br/>
     */
    @JsonProperty("base_url")
    private String baseUrl;
}

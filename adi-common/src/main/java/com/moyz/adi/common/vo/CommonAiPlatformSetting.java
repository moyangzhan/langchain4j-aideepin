package com.moyz.adi.common.vo;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

@Data
public class CommonAiPlatformSetting {
    
    /**
     * 默认为空，使用代码中对应模型的地址；<br/>
     * deepseek:<a href="https://api-docs.deepseek.com/zh-cn/">api文档</a>;https://api.deepseek.com 或 https://api.deepseek.com/v1；<br/>
     */
    @JsonProperty("base_url")
    private String baseUrl;
}

package com.moyz.adi.common.vo;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 硅基流动 设置
 *
 * @author pengh
 * @date 2025/04/16 13:56:47
 */
@EqualsAndHashCode(callSuper = true)
@Data
public class SiliconflowSetting extends CommonAiPlatformSetting {
    @JsonProperty("secret_key")
    private String secretKey;
}

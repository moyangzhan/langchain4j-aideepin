package com.moyz.adi.common.dto.mcp;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

/**
 * 已由系统管理员初始化完成的参数
 */
@Data
public class McpCommonParam {
    private String name;
    private String title;
    private Object value;
    @JsonProperty("require_encrypt")
    private Boolean requireEncrypt;
    private Boolean encrypted;
}

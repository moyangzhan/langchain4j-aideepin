package com.moyz.adi.common.dto.mcp;

import lombok.Data;

/**
 * 用户设置的MCP参数
 */
@Data
public class UserMcpCustomizedParam {
    private String name;
    private Object value;
    private Boolean encrypted;
}

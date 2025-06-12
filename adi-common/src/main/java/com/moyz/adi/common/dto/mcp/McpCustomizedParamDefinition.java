package com.moyz.adi.common.dto.mcp;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

/**
 * 待用户设置的MCP参数定义(个性化配置),用户设置后与mcp.common_params合并做为mcp的启动参数
 */
@Data
public class McpCustomizedParamDefinition {
    private String name;
    private String title;
    @JsonProperty("require_encrypt")
    private Boolean requireEncrypt;
}

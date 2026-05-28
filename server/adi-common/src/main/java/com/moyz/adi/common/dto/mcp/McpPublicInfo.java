package com.moyz.adi.common.dto.mcp;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Data
@Schema(title = "MCP公开信息 | MCP Public Info")
public class McpPublicInfo {

    @Schema(title = "uuid")
    private String uuid;

    @Schema(title = "标题 | Title")
    private String title;

    @Schema(title = "传输类型：sse, stdio | Transport type: sse, stdio")
    private String transportType;

    @Schema(title = "SSE URL")
    private String sseUrl;

    @Schema(title = "SSE超时时间 | SSE Timeout")
    private Integer sseTimeout;

    @Schema(title = "stdio命令 | stdio Command")
    private String stdioCommand;

    @Schema(title = "描述 | Description")
    private String remark;

    @Schema(title = "安装类型 | Install type")
    private String installType;

    @Schema(title = "是否启用 | Is enabled")
    private Boolean isEnable;
}

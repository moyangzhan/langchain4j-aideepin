package com.moyz.adi.common.dto.mcp;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class McpSearchReq {
    private String title;
    private Boolean isEnable;
    private String installType;
    private String transportType;
}

package com.moyz.adi.common.dto.mcp;

import lombok.Data;

import java.util.List;

@Data
public class McpListReq {
    private List<Long> ids;
}

package com.moyz.adi.common.dto;

import com.moyz.adi.common.dto.mcp.UserMcpCustomizedParam;
import com.moyz.adi.common.entity.Mcp;
import lombok.Data;

import java.util.List;

@Data
public class UserMcpDto {
    private Long id;

    private String uuid;

    private Long userId;

    private Long mcpId;

    private List<UserMcpCustomizedParam> mcpCustomizedParams;

    private Boolean isEnable;

    private Mcp mcpInfo;
}

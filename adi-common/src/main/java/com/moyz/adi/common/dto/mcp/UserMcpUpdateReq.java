package com.moyz.adi.common.dto.mcp;

import jakarta.validation.constraints.NotNull;
import lombok.Data;
import org.springframework.validation.annotation.Validated;

import java.util.List;

@Validated
@Data
public class UserMcpUpdateReq {
    @NotNull
    private Long mcpId;
    private List<UserMcpCustomizedParam> mcpCustomizedParams;
    private Boolean isEnable;
}

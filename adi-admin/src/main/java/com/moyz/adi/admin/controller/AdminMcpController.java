package com.moyz.adi.admin.controller;

import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.mcp.McpAddOrEditReq;
import com.moyz.adi.common.entity.Mcp;
import com.moyz.adi.common.service.McpService;
import jakarta.annotation.Resource;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

/**
 * MCP信息只能由系统管理员进行维护
 */
@RestController
@RequestMapping("/admin/mcp")
@Validated
public class AdminMcpController {

    @Resource
    private McpService mcpService;

    @PostMapping("/add")
    public Mcp save(@Validated @RequestBody McpAddOrEditReq commentAddReq) {
        return mcpService.addOrUpdate(commentAddReq, ThreadContext.getCurrentUser().getIsAdmin());
    }

    @PostMapping("/edit")
    public Mcp edit(@Validated @RequestBody McpAddOrEditReq commentAddReq) {
        return mcpService.addOrUpdate(commentAddReq, ThreadContext.getCurrentUser().getIsAdmin());
    }

    @PostMapping("/del/{uuid}")
    public boolean del(@PathVariable String uuid) {
        mcpService.softDelete(uuid);
        return true;
    }
}

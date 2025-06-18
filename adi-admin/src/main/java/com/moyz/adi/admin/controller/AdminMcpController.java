package com.moyz.adi.admin.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.mcp.McpAddOrEditReq;
import com.moyz.adi.common.dto.mcp.McpSearchReq;
import com.moyz.adi.common.entity.Mcp;
import com.moyz.adi.common.service.McpService;
import io.swagger.v3.oas.annotations.Operation;
import jakarta.annotation.Resource;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
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

    @Operation(summary = "搜索列表")
    @PostMapping(value = "/search")
    public Page<Mcp> search(@RequestBody McpSearchReq req, @NotNull @Min(1) Integer currentPage, @NotNull @Min(10) Integer pageSize) {
        return mcpService.search(req, currentPage, pageSize, true);
    }

    @PostMapping("/add")
    public Mcp save(@Validated @RequestBody McpAddOrEditReq commentAddReq) {
        return mcpService.addOrUpdate(commentAddReq, ThreadContext.getCurrentUser().getIsAdmin());
    }

    @PostMapping("/edit")
    public Mcp edit(@Validated @RequestBody McpAddOrEditReq commentAddReq) {
        return mcpService.addOrUpdate(commentAddReq, ThreadContext.getCurrentUser().getIsAdmin());
    }

    @PostMapping("/enable")
    public boolean enable(@RequestParam String uuid, @RequestParam Boolean isEnable) {
        mcpService.enable(uuid, isEnable);
        return true;
    }

    @PostMapping("/del/{uuid}")
    public boolean del(@PathVariable String uuid) {
        mcpService.softDelete(uuid);
        return true;
    }
}

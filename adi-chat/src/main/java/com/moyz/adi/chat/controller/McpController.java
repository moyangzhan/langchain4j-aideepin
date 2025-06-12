package com.moyz.adi.chat.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.dto.mcp.McpListReq;
import com.moyz.adi.common.entity.Mcp;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.service.McpService;
import io.swagger.v3.oas.annotations.Operation;
import jakarta.annotation.Resource;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/mcp")
@Validated
public class McpController {

    @Resource
    private McpService mcpService;

    @Operation(summary = "搜索列表")
    @GetMapping(value = "/public/search")
    public Page<Mcp> search(@RequestParam String keyword, @NotNull @Min(1) Integer currentPage, @NotNull @Min(10) Integer pageSize) {
        return mcpService.search(keyword, currentPage, pageSize, false);
    }

    @Operation(summary = "MCP列表")
    @GetMapping(value = "/public/list")
    public List<Mcp> list(@RequestBody McpListReq mcpListReq) {
        if (CollectionUtils.isEmpty(mcpListReq.getIds())) {
            return List.of();
        }
        if (mcpListReq.getIds().size() > 1000) {
            throw new BaseException(ErrorEnum.A_PARAMS_INVALID_BY_, "最多只能查询1000条数据");
        }
        return mcpService.listByIds(mcpListReq.getIds(), false);
    }

}

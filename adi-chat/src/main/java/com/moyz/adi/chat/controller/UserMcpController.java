package com.moyz.adi.chat.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.UserMcpDto;
import com.moyz.adi.common.dto.mcp.UserMcpUpdateReq;
import com.moyz.adi.common.service.UserMcpService;
import io.swagger.v3.oas.annotations.Operation;
import jakarta.annotation.Resource;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/user/mcp")
public class UserMcpController {

    @Resource
    private UserMcpService userMcpService;

    @Operation(summary = "当前登录用户启用的MCP列表")
    @GetMapping(value = "/list")
    public Page<UserMcpDto> listByUserId(@NotNull @Min(1) Integer currentPage, @NotNull @Min(10) Integer pageSize) {
        return userMcpService.searchByUserId(ThreadContext.getCurrentUserId(), currentPage, pageSize);
    }

    @PostMapping("/saveOrUpdate")
    public UserMcpDto saveOrUpdate(@Validated @RequestBody UserMcpUpdateReq userMcpUpdateReq) {
        return userMcpService.saveOrUpdate(userMcpUpdateReq);
    }

}

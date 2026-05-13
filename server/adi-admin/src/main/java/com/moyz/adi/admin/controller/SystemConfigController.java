package com.moyz.adi.admin.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.dto.SysConfigDto;
import com.moyz.adi.common.dto.SysConfigEditDto;
import com.moyz.adi.common.dto.SysConfigSearchReq;
import com.moyz.adi.common.entity.SysConfig;
import com.moyz.adi.common.service.SysConfigService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.annotation.Resource;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Tag(name = "系统配置管理 | System Config Management", description = "系统配置管理 | System Config Management")
@RestController
@RequestMapping("/admin/sys-config")
@Validated
public class SystemConfigController {

    @Resource
    private SysConfigService sysConfigService;

    @Operation(summary = "搜索系统配置 | Search System Configs")
    @PostMapping("/search")
    public Page<SysConfigDto> search(@RequestBody SysConfigSearchReq searchReq, @NotNull @Min(1) Integer currentPage, @NotNull @Min(10) Integer pageSize) {
        return sysConfigService.search(searchReq, currentPage, pageSize);
    }

    @Operation(summary = "编辑系统配置 | Edit System Config")
    @PostMapping("/edit")
    public boolean edit(@Validated @RequestBody SysConfigEditDto sysConfigDto) {
        return sysConfigService.edit(sysConfigDto) > 0;
    }

    @Operation(summary = "删除系统配置 | Delete System Config")
    @PostMapping("/del/{id}")
    public boolean delete(@PathVariable Long id) {
        return sysConfigService.softDelete(id);
    }
}

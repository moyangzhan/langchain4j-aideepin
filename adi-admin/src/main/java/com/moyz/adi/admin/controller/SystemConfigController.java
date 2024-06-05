package com.moyz.adi.admin.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.dto.SysConfigDto;
import com.moyz.adi.common.entity.SysConfig;
import com.moyz.adi.common.service.KnowledgeBaseService;
import com.moyz.adi.common.service.SysConfigService;
import jakarta.annotation.Resource;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/admin/sys-config")
@Validated
public class SystemConfigController {

    @Resource
    private SysConfigService sysConfigService;

    @GetMapping("/search")
    public Page<SysConfig> search(@RequestParam String keyword, @NotNull @Min(1) Integer currentPage, @NotNull @Min(10) Integer pageSize) {
        return sysConfigService.search(keyword, currentPage, pageSize);
    }

    @PostMapping("/edit")
    public void edit(@Validated @RequestBody SysConfigDto sysConfigDto) {
        sysConfigService.edit(sysConfigDto);
    }

    @PostMapping("/delete/{id}")
    public void delete(@PathVariable Long id) {
        sysConfigService.softDelete(id);
    }
}

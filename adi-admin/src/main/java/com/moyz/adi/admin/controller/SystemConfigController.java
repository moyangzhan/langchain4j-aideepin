package com.moyz.adi.admin.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.entity.SysConfig;
import com.moyz.adi.common.service.KnowledgeBaseService;
import com.moyz.adi.common.service.SysConfigService;
import jakarta.annotation.Resource;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/admin/sys-config")
@Validated
public class SystemConfigController {

    @Resource
    private SysConfigService sysConfigService;

    public Page<SysConfig> list(String keyword, @NotNull @Min(1) Integer currentPage, @NotNull @Min(10) Integer pageSize) {
        return sysConfigService.search(keyword, currentPage, pageSize);
    }
}

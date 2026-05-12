package com.moyz.adi.admin.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.dto.ModelPlatformSearchReq;
import com.moyz.adi.common.entity.ModelPlatform;
import com.moyz.adi.common.service.ModelPlatformService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.annotation.Resource;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

@Tag(name = "模型平台管理 | Model Platform Management", description = "模型平台管理 | Model Platform Management")
@RestController
@RequestMapping("/admin/model-platform/")
@Validated
public class ModelPlatformController {

    @Resource
    private ModelPlatformService modelPlatformService;

    @Operation(summary = "搜索模型平台 | Search Model Platforms")
    @PostMapping("/search")
    public Page<ModelPlatform> page(@RequestBody ModelPlatformSearchReq searchReq, @NotNull @Min(1) Integer currentPage, @NotNull @Min(10) Integer pageSize) {
        return modelPlatformService.search(searchReq, currentPage, pageSize);
    }

    @Operation(summary = "添加模型平台 | Add Model Platform")
    @PostMapping("/add")
    public ModelPlatform addOne(@RequestBody ModelPlatform modelPlatform) {
        return modelPlatformService.addOne(modelPlatform);
    }

    @Operation(summary = "编辑模型平台 | Edit Model Platform")
    @PostMapping("/edit")
    public void edit(@RequestBody ModelPlatform modelPlatform) {
        modelPlatformService.edit(modelPlatform);
    }

    @Operation(summary = "删除模型平台 | Delete Model Platform")
    @PostMapping("/del/{id}")
    public void delete(@PathVariable Long id) {
        modelPlatformService.softDelete(id);
    }
}

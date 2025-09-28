package com.moyz.adi.admin.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.dto.ModelPlatformSearchReq;
import com.moyz.adi.common.entity.ModelPlatform;
import com.moyz.adi.common.service.ModelPlatformService;
import jakarta.annotation.Resource;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/admin/model-platform/")
@Validated
public class ModelPlatformController {

    @Resource
    private ModelPlatformService modelPlatformService;

    @PostMapping("/search")
    public Page<ModelPlatform> page(@RequestBody ModelPlatformSearchReq searchReq, @NotNull @Min(1) Integer currentPage, @NotNull @Min(10) Integer pageSize) {
        return modelPlatformService.search(searchReq, currentPage, pageSize);
    }

    @PostMapping("/add")
    public ModelPlatform addOne(@RequestBody ModelPlatform modelPlatform) {
        return modelPlatformService.addOne(modelPlatform);
    }

    @PostMapping("/edit")
    public void edit(@RequestBody ModelPlatform modelPlatform) {
        modelPlatformService.edit(modelPlatform);
    }

    @PostMapping("/del/{id}")
    public void delete(@PathVariable Long id) {
        modelPlatformService.softDelete(id);
    }
}

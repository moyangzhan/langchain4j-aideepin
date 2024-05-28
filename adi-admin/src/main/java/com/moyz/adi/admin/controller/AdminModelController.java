package com.moyz.adi.admin.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.dto.AiModelDto;
import com.moyz.adi.common.service.AiModelService;
import jakarta.annotation.Resource;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/admin/model")
@Validated
public class AdminModelController {

    @Resource
    private AiModelService aiModelService;

    @GetMapping("/list")
    public Page<AiModelDto> page(@RequestParam String platform, @NotNull @Min(1) Integer currentPage, @NotNull @Min(10) Integer pageSize) {
        return aiModelService.page(platform, currentPage, pageSize);
    }

    @PostMapping("/disable/{id}")
    public void disable(@PathVariable Long id) {
        aiModelService.disable(id);
    }

    @PostMapping("/enable/{id}")
    public void enable(@PathVariable Long id) {
        aiModelService.enable(id);
    }

    @PostMapping("/delete/{id}")
    public void delete(@PathVariable Long id) {
        aiModelService.softDelete(id);
    }
}

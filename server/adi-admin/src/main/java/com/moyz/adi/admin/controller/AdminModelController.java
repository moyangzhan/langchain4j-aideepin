package com.moyz.adi.admin.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.dto.AiModelDto;
import com.moyz.adi.common.dto.AiModelSearchReq;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.interfaces.AiModelAddGroup;
import com.moyz.adi.common.interfaces.AiModelEditGroup;
import com.moyz.adi.common.service.AiModelService;
import com.moyz.adi.common.service.ModelHealthService;
import com.moyz.adi.common.util.AiModelUtil;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.annotation.Resource;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import org.apache.commons.lang3.StringUtils;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

import static com.moyz.adi.common.enums.ErrorEnum.A_PARAMS_ERROR;

@Tag(name = "AI模型管理 | AI Model Management", description = "AI模型管理 | AI Model Management")
@RestController
@RequestMapping("/admin/model")
@Validated
public class AdminModelController {

    @Resource
    private AiModelService aiModelService;

    @Resource
    private ModelHealthService modelHealthService;

    @Operation(summary = "搜索模型 | Search Models")
    @PostMapping("/search")
    public Page<AiModelDto> page(@RequestBody AiModelSearchReq aiModelSearchReq, @NotNull @Min(1) Integer currentPage, @NotNull @Min(10) Integer pageSize) {
        return aiModelService.search(aiModelSearchReq, currentPage, pageSize);
    }

    @Operation(summary = "添加模型 | Add Model")
    @PostMapping("/addOne")
    public AiModelDto addOne(@Validated(AiModelAddGroup.class) @RequestBody AiModelDto aiModelDto) {
        check(aiModelDto.getType());
        return aiModelService.addOne(aiModelDto);
    }

    @Operation(summary = "编辑模型 | Edit Model")
    @PostMapping("/edit")
    public void edit(@Validated(AiModelEditGroup.class) @RequestBody AiModelDto aiModelDto) {
        check(aiModelDto.getType());
        aiModelService.edit(aiModelDto);
    }

    @Operation(summary = "删除模型 | Delete Model")
    @PostMapping("/del/{id}")
    public void delete(@PathVariable Long id) {
        aiModelService.softDelete(id);
    }

    @Operation(summary = "模型健康状态 | Model Health Status")
    @GetMapping("/health")
    public Map<String, ModelHealthService.HealthCheckResult> health() {
        return modelHealthService.getAllStatuses();
    }

    @Operation(summary = "手动触发健康探测 | Trigger Health Check")
    @PostMapping("/health/check")
    public Map<String, ModelHealthService.HealthCheckResult> healthCheck() {
        modelHealthService.checkAll();
        return modelHealthService.getAllStatuses();
    }

    private void check(String type) {
        if (StringUtils.isNotBlank(type) && !AiModelUtil.checkModelType(type)) {
            throw new BaseException(A_PARAMS_ERROR);
        }
    }
}

package com.moyz.adi.admin.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.dto.workflow.WfComponentReq;
import com.moyz.adi.common.dto.workflow.WfComponentSearchReq;
import com.moyz.adi.common.entity.WorkflowComponent;
import com.moyz.adi.common.service.WorkflowComponentService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.annotation.Resource;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

@Tag(name = "工作流组件管理 | Workflow Component Management", description = "工作流组件管理 | Workflow Component Management")
@RestController
@RequestMapping("/admin/workflow/component")
@Validated
public class AdminWfComponentController {
    @Resource
    private WorkflowComponentService workflowComponentService;

    @Operation(summary = "搜索工作流组件 | Search Workflow Components")
    @PostMapping("/search")
    public Page<WorkflowComponent> search(@RequestBody WfComponentSearchReq searchReq, @NotNull @Min(1) Integer currentPage, @NotNull @Min(10) Integer pageSize) {
        return workflowComponentService.search(searchReq, currentPage, pageSize);
    }

    @Operation(summary = "启用/禁用工作流组件 | Enable/Disable Workflow Component")
    @PostMapping("/enable")
    public void enable(@RequestParam String uuid, @RequestParam Boolean isEnable) {
        workflowComponentService.enable(uuid, isEnable);
    }

    @Operation(summary = "删除工作流组件 | Delete Workflow Component")
    @PostMapping("/del/{uuid}")
    public void del(@PathVariable String uuid) {
        workflowComponentService.deleteByUuid(uuid);
    }


    @Operation(summary = "新增或更新工作流组件 | Add or Update Workflow Component")
    @PostMapping("/addOrUpdate")
    public WorkflowComponent addOrUpdate(@Validated @RequestBody WfComponentReq req) {
        return workflowComponentService.addOrUpdate(req);
    }

}

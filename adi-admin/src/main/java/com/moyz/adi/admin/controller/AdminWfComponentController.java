package com.moyz.adi.admin.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.dto.workflow.WfComponentReq;
import com.moyz.adi.common.dto.workflow.WfComponentSearchReq;
import com.moyz.adi.common.entity.WorkflowComponent;
import com.moyz.adi.common.service.WorkflowComponentService;
import jakarta.annotation.Resource;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/admin/workflow/component")
@Validated
public class AdminWfComponentController {
    @Resource
    private WorkflowComponentService workflowComponentService;

    @PostMapping("/search")
    public Page<WorkflowComponent> search(@RequestBody WfComponentSearchReq searchReq, @NotNull @Min(1) Integer currentPage, @NotNull @Min(10) Integer pageSize) {
        return workflowComponentService.search(searchReq, currentPage, pageSize);
    }

    @PostMapping("/enable")
    public void enable(@RequestParam String uuid, @RequestParam Boolean isEnable) {
        workflowComponentService.enable(uuid, isEnable);
    }

    @PostMapping("/del/{uuid}")
    public void del(@PathVariable String uuid) {
        workflowComponentService.deleteByUuid(uuid);
    }


    @PostMapping("/addOrUpdate")
    public WorkflowComponent addOrUpdate(@Validated @RequestBody WfComponentReq req) {
        return workflowComponentService.addOrUpdate(req);
    }

}

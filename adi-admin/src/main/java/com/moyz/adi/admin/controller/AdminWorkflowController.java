package com.moyz.adi.admin.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.dto.workflow.WfSearchReq;
import com.moyz.adi.common.dto.workflow.WorkflowResp;
import com.moyz.adi.common.service.WorkflowService;
import jakarta.annotation.Resource;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/admin/workflow")
@Validated
public class AdminWorkflowController {

    @Resource
    private WorkflowService workflowService;

    @PostMapping("/search")
    public Page<WorkflowResp> search(@RequestBody WfSearchReq req,
                                     @RequestParam @NotNull @Min(1) Integer currentPage,
                                     @RequestParam @NotNull @Min(10) Integer pageSize) {
        return workflowService.search(req.getTitle(), req.getIsPublic(), req.getIsEnable(), currentPage, pageSize);
    }

    @PostMapping("/enable")
    public void enable(@RequestParam String uuid, @RequestParam Boolean isEnable) {
        workflowService.enable(uuid, isEnable);
    }
}

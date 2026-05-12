package com.moyz.adi.admin.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.dto.workflow.WfSearchReq;
import com.moyz.adi.common.dto.workflow.WorkflowResp;
import com.moyz.adi.common.service.WorkflowService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.annotation.Resource;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

@Tag(name = "工作流管理 | Workflow Management", description = "工作流管理 | Workflow Management")
@RestController
@RequestMapping("/admin/workflow")
@Validated
public class AdminWorkflowController {

    @Resource
    private WorkflowService workflowService;

    @Operation(summary = "搜索工作流 | Search Workflows")
    @PostMapping("/search")
    public Page<WorkflowResp> search(@RequestBody WfSearchReq req,
                                     @RequestParam @NotNull @Min(1) Integer currentPage,
                                     @RequestParam @NotNull @Min(10) Integer pageSize) {
        return workflowService.search(req.getTitle(), req.getIsPublic(), req.getIsEnable(), currentPage, pageSize);
    }

    @Operation(summary = "启用/禁用工作流 | Enable/Disable Workflow")
    @PostMapping("/enable")
    public void enable(@RequestParam String uuid, @RequestParam Boolean isEnable) {
        workflowService.enable(uuid, isEnable);
    }
}

package com.moyz.adi.chat.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.dto.workflow.WfRuntimeNodeDto;
import com.moyz.adi.common.dto.workflow.WfRuntimeResp;
import com.moyz.adi.common.service.WorkflowRuntimeService;
import jakarta.annotation.Resource;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/workflow/runtime")
@Validated
public class WorkflowRuntimeController {

    @Resource
    private WorkflowRuntimeService workflowRuntimeService;

    @GetMapping("/page")
    public Page<WfRuntimeResp> search(@RequestParam String wfUuid,
                                      @NotNull @Min(1) Integer currentPage,
                                      @NotNull @Min(10) Integer pageSize) {
        return workflowRuntimeService.page(wfUuid, currentPage, pageSize);
    }

    @GetMapping("/nodes/{runtimeUuid}")
    public List<WfRuntimeNodeDto> listByRuntimeId(@PathVariable String runtimeUuid) {
        return workflowRuntimeService.listByRuntimeUuid(runtimeUuid);
    }

    @PostMapping("/clear")
    public boolean clear(@RequestParam(defaultValue = "") String wfUuid) {
        return workflowRuntimeService.deleteAll(wfUuid);
    }

    @PostMapping("/del/{wfRuntimeUuid}")
    public boolean delete(@PathVariable String wfRuntimeUuid) {
        return workflowRuntimeService.softDelete(wfRuntimeUuid);
    }
}

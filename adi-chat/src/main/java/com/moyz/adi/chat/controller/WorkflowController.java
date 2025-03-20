package com.moyz.adi.chat.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.workflow.*;
import com.moyz.adi.common.entity.WorkflowComponent;
import com.moyz.adi.common.service.WorkflowComponentService;
import com.moyz.adi.common.service.WorkflowService;
import com.moyz.adi.common.workflow.WorkflowEngine;
import com.moyz.adi.common.workflow.node.switcher.OperatorEnum;
import io.swagger.v3.oas.annotations.Operation;
import jakarta.annotation.Resource;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.util.*;

@RestController
@RequestMapping("/workflow")
@Validated
public class WorkflowController {

    @Resource
    private WorkflowEngine workflowEngine;

    @Resource
    private WorkflowService workflowService;

    @Resource
    private WorkflowComponentService workflowComponentService;

    @PostMapping("/add")
    public WorkflowResp add(@RequestBody @Validated WfAddReq addReq) {
        return workflowService.add(addReq.getTitle(), addReq.getRemark(), addReq.getIsPublic());
    }

    @PostMapping("/copy/{wfUuid}")
    public WorkflowResp copy(@PathVariable String wfUuid) {
        return workflowService.copy(wfUuid);
    }

    @PostMapping("/set-public/{wfUuid}")
    public void setPublic(@PathVariable String wfUuid) {
        workflowService.setPublic(wfUuid);
    }

    @PostMapping("/update")
    public WorkflowResp update(@RequestBody @Validated WorkflowUpdateReq req) {
        return workflowService.update(req);
    }

    @PostMapping("/del/{uuid}")
    public void delete(@PathVariable String uuid) {
        workflowService.softDelete(uuid);
    }

    @PostMapping("/base-info/update")
    public WorkflowResp updateBaseInfo(@RequestBody @Validated WfBaseInfoUpdateReq req) {
        return workflowService.updateBaseInfo(req.getUuid(), req.getTitle(), req.getRemark(), req.getIsPublic());
    }

    @Operation(summary = "流式响应")
    @PostMapping(value = "/run/{wfUuid}", produces = MediaType.TEXT_EVENT_STREAM_VALUE)
    public SseEmitter sseAsk(@PathVariable String wfUuid, @RequestBody WorkflowRunReq runReq) {
        return workflowEngine.streaming(ThreadContext.getCurrentUser(), wfUuid, runReq.getInputs());
    }

    @GetMapping("/mine/search")
    public Page<WorkflowResp> searchMine(@RequestParam(defaultValue = "") String keyword,
                                         @NotNull @Min(1) Integer currentPage,
                                         @NotNull @Min(10) Integer pageSize) {
        return workflowService.searchMine(keyword, currentPage, pageSize);
    }

    /**
     * 搜索公开工作流
     *
     * @param keyword     搜索关键词
     * @param currentPage 当前页数
     * @param pageSize    每页数量
     * @return 工作流列表
     */
    @GetMapping("/public/search")
    public Page<WorkflowResp> searchPublic(@RequestParam(defaultValue = "") String keyword,
                                           @NotNull @Min(1) Integer currentPage,
                                           @NotNull @Min(10) Integer pageSize) {
        return workflowService.searchPublic(keyword, currentPage, pageSize);
    }

    @GetMapping("/public/operators")
    public List<Map<String, String>> searchPublic() {
        List<Map<String, String>> result = new ArrayList<>();
        for (OperatorEnum operator : OperatorEnum.values()) {
            result.add(Map.of("name", operator.getName(), "desc", operator.getDesc()));
        }
        return result;
    }

    @GetMapping("/public/component/list")
    public List<WorkflowComponent> component() {
        return workflowComponentService.getAllEnable();
    }
}

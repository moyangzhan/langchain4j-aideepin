package com.moyz.adi.common.workflow;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.moyz.adi.common.entity.*;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.helper.SseManager;
import com.moyz.adi.common.util.SpringUtil;
import com.moyz.adi.common.service.*;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Lazy;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.util.List;
import java.util.Map;

import static com.moyz.adi.common.cosntant.AdiConstant.SSE_TIMEOUT;
import static com.moyz.adi.common.enums.ErrorEnum.*;

@Slf4j
@Component
public class WorkflowStarter {

    @Lazy
    @Resource
    private WorkflowStarter self;

    @Resource
    private WorkflowService workflowService;

    @Resource
    private WorkflowNodeService workflowNodeService;

    @Resource
    private WorkflowEdgeService workflowEdgeService;

    @Resource
    private WorkflowComponentService workflowComponentService;

    @Resource
    private WorkflowRuntimeService workflowRuntimeService;

    @Resource
    private WorkflowRuntimeNodeService workflowRuntimeNodeService;

    @Resource
    private SseManager sseManager;


    public SseEmitter streaming(User user, String workflowUuid, List<ObjectNode> userInputs) {
        String sseUuid = com.moyz.adi.common.util.UuidUtil.createShort();
        SseEmitter sseEmitter = new SseEmitter(SSE_TIMEOUT);
        if (!sseManager.checkOrComplete(user, sseUuid, sseEmitter)) {
            return sseEmitter;
        }
        // 工作流的 START 事件由异步线程在 WorkflowEngine.run 中发送（需携带 wfRuntimeResp 数据），
        // 所以这里不能像聊天那样直接调 startSse；只注册 emitter，供异步线程通过 sseUuid 取回使用。
        // <p>
        // The workflow START event is sent from the async thread inside WorkflowEngine.run (it
        // carries the wfRuntimeResp payload), so unlike chat we cannot call startSse here; we only
        // register the emitter so the async thread can look it up by sseUuid.
        sseManager.register(sseUuid, sseEmitter, user.getId());
        Workflow workflow = workflowService.getByUuid(workflowUuid);
        if (null == workflow) {
            sseManager.sendErrorAndComplete(user.getId(), sseUuid, SpringUtil.getMessage(A_WF_NOT_FOUND.getInfo()));
            return sseEmitter;
        } else if (Boolean.FALSE.equals(workflow.getIsEnable())) {
            sseManager.sendErrorAndComplete(user.getId(), sseUuid, SpringUtil.getMessage(A_WF_DISABLED.getInfo()));
            return sseEmitter;
        }
        self.asyncRun(user, workflow, userInputs, sseUuid);
        return sseEmitter;
    }

    @Async
    public void asyncRun(User user, Workflow workflow, List<ObjectNode> userInputs, String sseUuid) {
        log.info("WorkflowEngine run,userId:{},workflowUuid:{},userInputs:{}", user.getId(), workflow.getUuid(), userInputs);
        try {
            List<WorkflowComponent> components = workflowComponentService.getAllEnable();
            List<WorkflowNode> nodes = workflowNodeService.lambdaQuery()
                    .eq(WorkflowNode::getWorkflowId, workflow.getId())
                    .eq(WorkflowNode::getIsDeleted, false)
                    .list();
            List<WorkflowEdge> edges = workflowEdgeService.lambdaQuery()
                    .eq(WorkflowEdge::getWorkflowId, workflow.getId())
                    .eq(WorkflowEdge::getIsDeleted, false)
                    .list();
            WorkflowEngine workflowEngine = new WorkflowEngine(workflow,
                    sseManager,
                    components,
                    nodes,
                    edges,
                    workflowRuntimeService,
                    workflowRuntimeNodeService);
            workflowEngine.run(user, userInputs, sseUuid);
        } catch (Throwable e) {
            log.error("asyncRun execution exception, workflowUuid:{}", workflow.getUuid(), e);
            sseManager.sendErrorAndComplete(user.getId(), sseUuid, "Workflow execution error:" + e.getMessage());
        }
    }

    /**
     * Run a workflow in blocking mode, wait for completion and return the result.
     */
    public Map<String, Object> blocking(User user, String workflowUuid, List<ObjectNode> userInputs) {
        Workflow workflow = workflowService.getByUuid(workflowUuid);
        if (null == workflow) {
            throw new BaseException(A_WF_NOT_FOUND);
        } else if (Boolean.FALSE.equals(workflow.getIsEnable())) {
            throw new BaseException(A_WF_DISABLED);
        }

        List<WorkflowComponent> components = workflowComponentService.getAllEnable();
        List<WorkflowNode> nodes = workflowNodeService.lambdaQuery()
                .eq(WorkflowNode::getWorkflowId, workflow.getId())
                .eq(WorkflowNode::getIsDeleted, false)
                .list();
        List<WorkflowEdge> edges = workflowEdgeService.lambdaQuery()
                .eq(WorkflowEdge::getWorkflowId, workflow.getId())
                .eq(WorkflowEdge::getIsDeleted, false)
                .list();

        WorkflowEngine workflowEngine = new WorkflowEngine(workflow,
                sseManager,
                components,
                nodes,
                edges,
                workflowRuntimeService,
                workflowRuntimeNodeService);
        return workflowEngine.blockingRun(user, userInputs);
    }

    @Async
    public void resumeFlow(String runtimeUuid, String userInput) {
        WorkflowEngine workflowEngine = InterruptedFlow.get(runtimeUuid);
        if (null == workflowEngine) {
            log.error("Workflow resume execution failed, runtime:{}", runtimeUuid);
            throw new BaseException(A_WF_RESUME_FAIL);
        }
        try {
            workflowEngine.resume(userInput);
        } catch (Throwable e) {
            log.error("resumeFlow execution exception, runtimeUuid:{}", runtimeUuid, e);
        }
    }

}

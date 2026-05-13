package com.moyz.adi.common.workflow;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.moyz.adi.common.entity.*;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.helper.SSEEmitterHelper;
import com.moyz.adi.common.util.SpringUtil;
import com.moyz.adi.common.service.*;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.util.List;

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
    private SSEEmitterHelper sseEmitterHelper;


    public SseEmitter streaming(User user, String workflowUuid, List<ObjectNode> userInputs) {
        SseEmitter sseEmitter = new SseEmitter(SSE_TIMEOUT);
        if (!sseEmitterHelper.checkOrComplete(user, sseEmitter)) {
            return sseEmitter;
        }
        Workflow workflow = workflowService.getByUuid(workflowUuid);
        if (null == workflow) {
            sseEmitterHelper.sendErrorAndComplete(user.getId(), sseEmitter, SpringUtil.getMessage(A_WF_NOT_FOUND.getInfo()));
            return sseEmitter;
        } else if (Boolean.FALSE.equals(workflow.getIsEnable())) {
            sseEmitterHelper.sendErrorAndComplete(user.getId(), sseEmitter, SpringUtil.getMessage(A_WF_DISABLED.getInfo()));
            return sseEmitter;
        }
        self.asyncRun(user, workflow, userInputs, sseEmitter);
        return sseEmitter;
    }

    @Async
    public void asyncRun(User user, Workflow workflow, List<ObjectNode> userInputs, SseEmitter sseEmitter) {
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
                    sseEmitterHelper,
                    components,
                    nodes,
                    edges,
                    workflowRuntimeService,
                    workflowRuntimeNodeService);
            workflowEngine.run(user, userInputs, sseEmitter);
        } catch (Throwable e) {
            log.error("asyncRun execution exception, workflowUuid:{}", workflow.getUuid(), e);
            sseEmitterHelper.sendErrorAndComplete(user.getId(), sseEmitter, "Workflow execution error:" + e.getMessage());
        }
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

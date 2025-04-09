package com.moyz.adi.common.workflow;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.moyz.adi.common.entity.*;
import com.moyz.adi.common.helper.SSEEmitterHelper;
import com.moyz.adi.common.service.*;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.util.List;

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
        SseEmitter sseEmitter = new SseEmitter();
        if (!sseEmitterHelper.checkOrComplete(user, sseEmitter)) {
            return sseEmitter;
        }
        self.asyncRun(user, workflowUuid, userInputs, sseEmitter);
        return sseEmitter;
    }

    @Async
    public void asyncRun(User user, String workflowUuid, List<ObjectNode> userInputs, SseEmitter sseEmitter) {
        log.info("WorkflowEngine run,userId:{},workflowUuid:{},userInputs:{}", user.getId(), workflowUuid, userInputs);
        Workflow workflow = workflowService.getByUuid(workflowUuid);
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
    }

}

package com.moyz.adi.common.workflow;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.moyz.adi.common.dto.workflow.WfRuntimeMetricsSummary;
import com.moyz.adi.common.dto.workflow.WfRuntimeNodeDto;
import com.moyz.adi.common.entity.*;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.helper.SseManager;
import com.moyz.adi.common.util.SpringUtil;
import com.moyz.adi.common.util.UuidUtil;
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
import static com.moyz.adi.common.cosntant.AdiConstant.WorkflowConstant.WAITING_INPUT_TIMEOUT_MARK;
import static com.moyz.adi.common.cosntant.AdiConstant.WorkflowConstant.WORKFLOW_PROCESS_STATUS_FAIL;
import static com.moyz.adi.common.cosntant.AdiConstant.WorkflowConstant.WORKFLOW_PROCESS_STATUS_WAITING_INPUT;
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
        String sseUuid = UuidUtil.createShort();
        SseEmitter sseEmitter = new SseEmitter(SSE_TIMEOUT);
        if (!sseManager.checkOrComplete(user, sseUuid, sseEmitter)) {
            return sseEmitter;
        }
        //START is sent by the async run (it carries the runtime payload); only register the emitter here
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

    /**
     * 续跑等待输入的工作流：任意实例可服务——从 PG 的 checkpoint 重建引擎，输出走 resume 请求自带的新流
     * <p>
     * Resume a waiting-input workflow; ANY instance can serve — the engine is rebuilt from the persisted checkpoint
     * and output streams on the resume request's own NEW SSE stream (the interrupted original stream was closed without a terminal event at interruption time).
     * </p>
     */
    public SseEmitter resume(User user, String runtimeUuid, String feedbackContent) {
        //Create the stream before the CAS so rejections can ride it as [ERROR] without touching state
        String sseUuid = UuidUtil.createShort();
        SseEmitter sseEmitter = new SseEmitter(SSE_TIMEOUT);
        if (!sseManager.checkOrComplete(user, sseUuid, sseEmitter)) {
            return sseEmitter;
        }
        WorkflowRuntime runtime = null;
        boolean casPassed = false;
        try {
            //Ownership is enforced on the request thread; the async continuation reuses the row
            runtime = workflowRuntimeService.getForResume(runtimeUuid);
            if (null == runtime) {
                throw new BaseException(A_WF_RESUME_FAIL);
            }
            if (runtime.getStatus() != WORKFLOW_PROCESS_STATUS_WAITING_INPUT) {
                if (runtime.getStatus() == WORKFLOW_PROCESS_STATUS_FAIL
                        && null != runtime.getStatusRemark()
                        && runtime.getStatusRemark().contains(WAITING_INPUT_TIMEOUT_MARK)) {
                    throw new BaseException(A_WF_RESUME_EXPIRED);
                }
                throw new BaseException(A_WF_RESUME_FAIL);
            }
            if (!workflowRuntimeService.casWaitingInputToRunning(runtimeUuid)) {
                throw new BaseException(A_WF_RESUME_DUPLICATE);
            }
            casPassed = true;
            sseManager.register(sseUuid, sseEmitter, user.getId());
            self.asyncResume(user, runtime, feedbackContent, sseUuid);
        } catch (Exception e) {
            log.error("Workflow resume rejected, runtimeUuid:{}", runtimeUuid, e);
            //CAS won but takeover failed; fail the row (pre-CAS rejections leave it resumable)
            if (casPassed && null != runtime) {
                failRuntimeAfterCAS(runtime, e);
            }
            //A [WF_RESUME_*] prefix marks a soft rejection the client downgrades to a light hint
            String msg;
            if (e instanceof BaseException be) {
                msg = SpringUtil.getMessage(be.getInfo());
                if (A_WF_RESUME_DUPLICATE.getInfo().equals(be.getInfo())) {
                    msg = "[WF_RESUME_DUPLICATE]" + msg;
                } else if (A_WF_RESUME_EXPIRED.getInfo().equals(be.getInfo())) {
                    msg = "[WF_RESUME_EXPIRED]" + msg;
                }
            } else {
                msg = e.getMessage();
            }
            sseManager.sendErrorAndComplete(user.getId(), sseUuid, msg);
        }
        return sseEmitter;
    }

    @Async
    public void asyncResume(User user, WorkflowRuntime runtime, String feedbackContent, String sseUuid) {
        try {
            if (null == runtime.getCheckpoint()) {
                log.error("Resume checkpoint missing, runtimeUuid:{}", runtime.getUuid());
                throw new BaseException(A_WF_RESUME_FAIL);
            }
            Workflow workflow = workflowService.getById(runtime.getWorkflowId());
            if (null == workflow || workflow.getIsDeleted()) {
                throw new BaseException(A_WF_NOT_FOUND);
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
            //No ThreadContext on the @Async thread; ownership was enforced on the request thread
            List<WfRuntimeNodeDto> runtimeNodeDtos = workflowRuntimeNodeService.listByRuntimeIdForResume(runtime.getId());
            WorkflowEngine workflowEngine = new WorkflowEngine(workflow,
                    sseManager,
                    components,
                    nodes,
                    edges,
                    workflowRuntimeService,
                    workflowRuntimeNodeService);
            workflowEngine.resume(user, runtime, runtime.getCheckpoint(), runtimeNodeDtos, feedbackContent, sseUuid);
        } catch (Throwable e) {
            log.error("asyncResume execution exception, runtimeUuid:{}", runtime.getUuid(), e);
            //Pre-engine failures fail the row here; engine-internal ones are handled by errorWhenExe
            failRuntimeAfterCAS(runtime, e);
            sseManager.sendErrorAndComplete(user.getId(), sseUuid, "Workflow resume error:" + e.getMessage());
        }
    }

    /**
     * 将运行置为 FAIL 并清空 checkpoint，保留行上已持久化的 token 统计。
     * Mark the runtime FAILED and clear its checkpoint, keeping the persisted token stats.
     */
    private void failRuntimeAfterCAS(WorkflowRuntime runtime, Throwable cause) {
        try {
            long runtimeCreateTime = WorkflowEngine.toEpochMillis(runtime.getCreateTime());
            WfRuntimeMetricsSummary preserved = new WfRuntimeMetricsSummary(
                    null == runtime.getInputTokens() ? 0 : runtime.getInputTokens(),
                    null == runtime.getOutputTokens() ? 0 : runtime.getOutputTokens(),
                    null == runtime.getDuration() ? 0 : runtime.getDuration());
            workflowRuntimeService.updateStatusWithCheckpoint(runtime.getId(),
                    WORKFLOW_PROCESS_STATUS_FAIL, "resume failed:" + cause.getMessage(), runtimeCreateTime,
                    preserved);
        } catch (Throwable persistEx) {
            log.error("Failed to persist resume failure status, runtimeUuid:{}", runtime.getUuid(), persistEx);
        }
    }

}

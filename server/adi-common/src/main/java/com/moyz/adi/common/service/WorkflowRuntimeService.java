package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.spring.service.impl.ServiceImpl;
import com.baomidou.mybatisplus.extension.toolkit.ChainWrappers;
import tools.jackson.databind.node.ObjectNode;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.workflow.WfRuntimeMetricsSummary;
import com.moyz.adi.common.dto.workflow.WfRuntimeNodeDto;
import com.moyz.adi.common.dto.workflow.WfRuntimeResp;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.entity.Workflow;
import com.moyz.adi.common.entity.WorkflowRuntime;
import com.moyz.adi.common.entity.WorkflowRuntimeNode;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.mapper.WorkflowRunMapper;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.util.MPPageUtil;
import com.moyz.adi.common.util.NumberUtil;
import com.moyz.adi.common.util.PrivilegeUtil;
import com.moyz.adi.common.util.UuidUtil;
import com.moyz.adi.common.workflow.WfNodeIODataUtil;
import com.moyz.adi.common.workflow.WfState;
import com.moyz.adi.common.workflow.data.NodeIOData;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import static com.moyz.adi.common.cosntant.AdiConstant.WorkflowConstant.WAITING_INPUT_TIMEOUT_MARK;
import static com.moyz.adi.common.cosntant.AdiConstant.WorkflowConstant.WORKFLOW_PROCESS_STATUS_DOING;

@Slf4j
@Service
public class WorkflowRuntimeService extends ServiceImpl<WorkflowRunMapper, WorkflowRuntime> {

    @Resource
    private WorkflowService workflowService;

    @Resource
    private WorkflowRuntimeNodeService workflowRuntimeNodeService;

    @Resource
    private FileService fileService;

    @Resource
    private FileGcService fileGcService;

    public WfRuntimeResp create(User user, Long workflowId) {
        WorkflowRuntime one = new WorkflowRuntime();
        one.setUuid(UuidUtil.createShort());
        one.setUserId(user.getId());
        one.setWorkflowId(workflowId);
        baseMapper.insert(one);

        one = baseMapper.selectById(one.getId());
        return changeToDTO(one);
    }

    /**
     * 运行建立时持久化 input 并登记其中 type=4 文件参数的引用（每次运行恰好写入一次，resume 重放不经过此处）。
     * <p>
     * Persist the run input and register references for its type=4 file parameters
     * (written exactly once per run; the resume replay does not go through here).
     * </p>
     */
    public void updateInput(long id, WfState wfState) {
        if (CollectionUtils.isEmpty(wfState.getInput())) {
            log.warn("No input data, id:{}", id);
            return;
        }
        WorkflowRuntime node = baseMapper.selectById(id);
        if (null == node) {
            log.error("Workflow runtime not found, id:{}", id);
            return;
        }
        WorkflowRuntime updateOne = new WorkflowRuntime();
        updateOne.setId(id);
        ObjectNode ob = JsonUtil.createObjectNode();
        for (NodeIOData data : wfState.getInput()) {
            ob.set(data.getName(), JsonUtil.classToJsonNode(data.getContent()));
        }
        updateOne.setInput(ob);
        updateOne.setStatus(WORKFLOW_PROCESS_STATUS_DOING);
        baseMapper.updateById(updateOne);
        fileService.attachRefs(WfNodeIODataUtil.extractFileUuids(ob));
    }

    /**
     * Persist the run output and a terminal metrics snapshot. Returns the same WorkflowRuntime
     * instance with all written fields populated (id, output, status, snapshot columns) so the
     * caller can push it via SSE without an extra round-trip; null only if no row matched the id.
     *
     * @param runtimeCreateTime epoch millis of the runtime row's create time, used for wall-clock duration
     * @param tokenSummary     pre-computed token totals from the caller (engine has them in memory)
     */
    public WorkflowRuntime updateOutput(long id, WfState wfState, long runtimeCreateTime, WfRuntimeMetricsSummary tokenSummary) {
        WorkflowRuntime updateOne = new WorkflowRuntime();
        updateOne.setId(id);
        ObjectNode ob = JsonUtil.createObjectNode();
        for (NodeIOData data : wfState.getOutput()) {
            ob.set(data.getName(), JsonUtil.classToJsonNode(data.getContent()));
        }
        updateOne.setOutput(ob);
        updateOne.setStatus(wfState.getProcessStatus());
        applyMetricsSnapshot(updateOne, runtimeCreateTime, tokenSummary);
        if (baseMapper.updateById(updateOne) == 0) {
            log.error("Workflow runtime not found, id:{}", id);
            return null;
        }
        return updateOne;
    }

    /**
     * Update status and persist a terminal metrics snapshot. Returns the same WorkflowRuntime
     * instance carrying status / status_remark / snapshot columns so the caller can push it via
     * SSE without an extra round-trip; null only if no row matched the id.
     *
     * @param runtimeCreateTime epoch millis of the runtime row's create time, used for wall-clock duration
     * @param tokenSummary     pre-computed token totals from the caller
     */
    public WorkflowRuntime updateStatus(long id, int processStatus, String statusRemark, long runtimeCreateTime, WfRuntimeMetricsSummary tokenSummary) {
        WorkflowRuntime updateOne = new WorkflowRuntime();
        updateOne.setId(id);
        updateOne.setStatus(processStatus);
        updateOne.setStatusRemark(StringUtils.substring(statusRemark, 0, 250));
        applyMetricsSnapshot(updateOne, runtimeCreateTime, tokenSummary);
        if (baseMapper.updateById(updateOne) == 0) {
            log.error("Workflow runtime not found, id:{}", id);
            return null;
        }
        return updateOne;
    }

    /**
     * Load the runtime row WITH the checkpoint blob for resume, enforcing ownership (owner or admin).
     * Must run on the request thread (ThreadContext-scoped); the async continuation passes the loaded row along.
     * 校验属主后返回含 checkpoint 的整行（仅 resume 重建用）。
     */
    public WorkflowRuntime getForResume(String uuid) {
        WorkflowRuntime runtime = baseMapper.selectWithCheckpoint(uuid);
        User current = ThreadContext.getCurrentUser();
        if (null == runtime || null == current
                || (!current.getIsAdmin() && !runtime.getUserId().equals(current.getId()))) {
            return null;
        }
        return runtime;
    }

    /**
     * Resume concurrency gate: conditional WAITING_INPUT -> DOING in one UPDATE —
     * the winner proceeds, duplicate submits are rejected atomically.
     */
    public boolean casWaitingInputToRunning(String uuid) {
        return baseMapper.casWaitingInputToRunning(uuid) == 1;
    }

    /**
     * Persist run output, a metrics snapshot AND the checkpoint column in ONE update.
     * A null checkpoint explicitly CLEARS the column (terminal statuses);
     * writer-based SQL keeps the "waiting_input row always carries a recoverable checkpoint / terminal row carries none" invariant
     * that entity-based updateById (skip-null) would silently break.
     */
    public WorkflowRuntime updateOutputWithCheckpoint(long id, WfState wfState, long runtimeCreateTime, WfRuntimeMetricsSummary tokenSummary, byte[] checkpoint) {
        WorkflowRuntime updateOne = new WorkflowRuntime();
        updateOne.setId(id);
        ObjectNode ob = JsonUtil.createObjectNode();
        for (NodeIOData data : wfState.getOutput()) {
            ob.set(data.getName(), JsonUtil.classToJsonNode(data.getContent()));
        }
        updateOne.setOutput(ob);
        updateOne.setStatus(wfState.getProcessStatus());
        applyMetricsSnapshot(updateOne, runtimeCreateTime, tokenSummary);
        updateOne.setCheckpoint(checkpoint);
        if (baseMapper.updateOutputWithCheckpoint(updateOne) == 0) {
            log.error("Workflow runtime not found, id:{}", id);
            return null;
        }
        return updateOne;
    }

    /**
     * Terminal status update that always CLEARS the checkpoint column (failure / success after a resumed run).
     * See {@link #updateOutputWithCheckpoint} for the explicit-clear rationale.
     */
    public WorkflowRuntime updateStatusWithCheckpoint(long id, int processStatus, String statusRemark, long runtimeCreateTime, WfRuntimeMetricsSummary tokenSummary) {
        WorkflowRuntime updateOne = new WorkflowRuntime();
        updateOne.setId(id);
        updateOne.setStatus(processStatus);
        updateOne.setStatusRemark(StringUtils.substring(statusRemark, 0, 250));
        applyMetricsSnapshot(updateOne, runtimeCreateTime, tokenSummary);
        if (baseMapper.updateStatusWithCheckpoint(updateOne) == 0) {
            log.error("Workflow runtime not found, id:{}", id);
            return null;
        }
        return updateOne;
    }

    /**
     * Timeout expiry: fail runtimes stuck in WAITING_INPUT past the threshold and clear their checkpoint — an expired run is not resumable.
     * update_time marks the interruption moment.
     * 超时回收 WAITING_INPUT：置失败并清空快照；被回收的运行不可再续跑（update_time=中断时刻）。
     */
    public int expireWaitingInput(int timeoutMinutes) {
        return baseMapper.expireWaitingInput(LocalDateTime.now().minusMinutes(timeoutMinutes), WAITING_INPUT_TIMEOUT_MARK);
    }

    public WorkflowRuntime getByUuid(String uuid) {
        return ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(!ThreadContext.getCurrentUser().getIsAdmin(), WorkflowRuntime::getUserId, ThreadContext.getCurrentUserId())
                .eq(WorkflowRuntime::getUuid, uuid)
                .eq(WorkflowRuntime::getIsDeleted, false)
                .last("limit 1")
                .one();
    }

    public Page<WfRuntimeResp> page(String wfUuid, Integer currentPage, Integer pageSize) {
        Workflow workflow = workflowService.getOrThrow(wfUuid);
        User user = ThreadContext.getCurrentUser();
        Page<WorkflowRuntime> page = ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(WorkflowRuntime::getWorkflowId, workflow.getId())
                .eq(WorkflowRuntime::getIsDeleted, false)
                .eq(!user.getIsAdmin(), WorkflowRuntime::getUserId, user.getId())
                .orderByDesc(WorkflowRuntime::getUpdateTime)
                .page(new Page<>(currentPage, pageSize));
        Page<WfRuntimeResp> result = new Page<>();
        MPPageUtil.convertToPage(page, result, WfRuntimeResp.class, (source, target) -> {
            fillInputOutput(target);
            fillMetricsFromEntity(source, target);
            return target;
        });
        return result;
    }

    public List<WfRuntimeNodeDto> listByRuntimeUuid(String runtimeUuid) {
        WorkflowRuntime runtime = PrivilegeUtil.checkAndGetByUuid(runtimeUuid, this.query(), ErrorEnum.A_WF_RUNTIME_NOT_FOUND);
        return workflowRuntimeNodeService.listByWfRuntimeId(runtime.getId());
    }

    /**
     * 清空当前用户（管理员为全部）某工作流的运行记录：先解除各运行 input 的文件引用再软删（软删即解除）。
     * <p>
     * Clear the current user's (all rows for an admin) runtimes of a workflow: detach each run's
     * input file references first, then soft-delete (a soft delete is a detach).
     * </p>
     */
    public boolean deleteAll(String wfUuid) {
        Workflow workflow = workflowService.getOrThrow(wfUuid);
        User user = ThreadContext.getCurrentUser();
        List<WorkflowRuntime> runtimes = ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(WorkflowRuntime::getWorkflowId, workflow.getId())
                .eq(!user.getIsAdmin(), WorkflowRuntime::getUserId, user.getId())
                .eq(WorkflowRuntime::getIsDeleted, false)
                .list();
        List<String> fileUuids = runtimes.stream()
                .flatMap(runtime -> WfNodeIODataUtil.extractFileUuids(runtime.getInput()).stream())
                .toList();
        fileService.detachRefs(fileUuids);
        boolean deleted = ChainWrappers.lambdaUpdateChain(baseMapper)
                .eq(WorkflowRuntime::getWorkflowId, workflow.getId())
                .eq(!user.getIsAdmin(), WorkflowRuntime::getUserId, user.getId())
                .set(WorkflowRuntime::getIsDeleted, true)
                .update();
        if (deleted) {
            clampRefsAfterRuntimesDeleted(runtimes);
        }
        return deleted;
    }

    private WfRuntimeResp changeToDTO(WorkflowRuntime runtime) {
        WfRuntimeResp result = new WfRuntimeResp();
        BeanUtils.copyProperties(runtime, result);
        fillInputOutput(result);
        fillMetricsFromEntity(runtime, result);
        return result;
    }

    private void fillInputOutput(WfRuntimeResp target) {
        if (null == target.getInput()) {
            target.setInput(JsonUtil.createObjectNode());
        }
        if (null == target.getOutput()) {
            target.setOutput(JsonUtil.createObjectNode());
        }
    }

    /**
     * Apply the pre-computed token summary and wall-clock duration onto the update DTO.
     */
    private void applyMetricsSnapshot(WorkflowRuntime updateOne, long runtimeCreateTime, WfRuntimeMetricsSummary summary) {
        updateOne.setInputTokens(NumberUtil.saturatedCastToInt(summary.inputTokens()));
        updateOne.setOutputTokens(NumberUtil.saturatedCastToInt(summary.outputTokens()));
        long wallClockMs = Math.max(0, System.currentTimeMillis() - runtimeCreateTime);
        updateOne.setDuration(NumberUtil.saturatedCastToInt(wallClockMs));
    }

    /**
     * Copy the persisted flat snapshot onto the resp DTO. Null columns leave the resp fields null
     * so the UI hides them (e.g. still DOING, or legacy row).
     */
    private void fillMetricsFromEntity(WorkflowRuntime source, WfRuntimeResp target) {
        target.setInputTokens(source.getInputTokens());
        target.setOutputTokens(source.getOutputTokens());
        target.setDuration(source.getDuration());
    }

    /**
     * 软删单条运行并解除其 input 的文件引用（软删即解除）。
     * <p>
     * Soft-delete one runtime and detach its input file references (a soft delete is a detach).
     * </p>
     */
    public boolean softDelete(String uuid) {
        WorkflowRuntime workflowRuntime = PrivilegeUtil.checkAndGetByUuid(uuid, this.query(), ErrorEnum.A_WF_NOT_FOUND);
        fileService.detachRefs(WfNodeIODataUtil.extractFileUuids(workflowRuntime.getInput()));
        boolean deleted = ChainWrappers.lambdaUpdateChain(baseMapper)
                .eq(WorkflowRuntime::getId, workflowRuntime.getId())
                .set(WorkflowRuntime::getIsDeleted, true)
                .update();
        if (deleted) {
            clampRefsAfterRuntimesDeleted(List.of(workflowRuntime));
        }
        return deleted;
    }

    /**
     * 运行删除后把涉及文件（运行 input/output 与其节点 input/output 中的 type=4 值）的 ref_count
     * 钳制为实查值：产物以 URL 形态引用、只增不减且无 detach 路径，须在引用消散点实查收敛。
     * <p>
     * After a runtime is deleted, clamp the ref_count of every touched file (type=4 values in the
     * run input/output and its nodes' input/output) to the live recount: product references are
     * URL-shaped, count up only and have no detach path, so they must converge at the point where
     * they dissolve.
     * </p>
     */
    private void clampRefsAfterRuntimesDeleted(List<WorkflowRuntime> runtimes) {
        List<String> uuids = new ArrayList<>();
        for (WorkflowRuntime runtime : runtimes) {
            uuids.addAll(WfNodeIODataUtil.extractFileUuidsForRecount(runtime.getInput()));
            uuids.addAll(WfNodeIODataUtil.extractFileUuidsForRecount(runtime.getOutput()));
        }
        List<Long> runtimeIds = runtimes.stream().map(WorkflowRuntime::getId).toList();
        for (WorkflowRuntimeNode node : workflowRuntimeNodeService.listByRuntimeIds(runtimeIds)) {
            uuids.addAll(WfNodeIODataUtil.extractFileUuidsForRecount(node.getInput()));
            uuids.addAll(WfNodeIODataUtil.extractFileUuidsForRecount(node.getOutput()));
        }
        fileGcService.clampRefCounts(uuids);
    }
}

package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.baomidou.mybatisplus.extension.toolkit.ChainWrappers;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.workflow.WfRuntimeMetricsSummary;
import com.moyz.adi.common.dto.workflow.WfRuntimeNodeDto;
import com.moyz.adi.common.dto.workflow.WfRuntimeResp;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.entity.Workflow;
import com.moyz.adi.common.entity.WorkflowRuntime;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.mapper.WorkflowRunMapper;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.util.MPPageUtil;
import com.moyz.adi.common.util.NumberUtil;
import com.moyz.adi.common.util.PrivilegeUtil;
import com.moyz.adi.common.util.UuidUtil;
import com.moyz.adi.common.workflow.WfState;
import com.moyz.adi.common.workflow.data.NodeIOData;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.List;

import static com.moyz.adi.common.cosntant.AdiConstant.WorkflowConstant.WORKFLOW_PROCESS_STATUS_DOING;

@Slf4j
@Service
public class WorkflowRuntimeService extends ServiceImpl<WorkflowRunMapper, WorkflowRuntime> {

    @Resource
    private WorkflowService workflowService;

    @Resource
    private WorkflowRuntimeNodeService workflowRuntimeNodeService;

    public WfRuntimeResp create(User user, Long workflowId) {
        WorkflowRuntime one = new WorkflowRuntime();
        one.setUuid(UuidUtil.createShort());
        one.setUserId(user.getId());
        one.setWorkflowId(workflowId);
        baseMapper.insert(one);

        one = baseMapper.selectById(one.getId());
        return changeToDTO(one);
    }

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
    }

    /**
     * Persist the run output and a terminal metrics snapshot. Returns the same WorkflowRuntime
     * instance with all written fields populated (id, output, status, snapshot columns) so the
     * caller can push it via SSE without an extra round-trip; null only if no row matched the id.
     *
     * @param runStartedMillis epoch millis when the run started, used for wall-clock duration
     * @param tokenSummary     pre-computed token totals from the caller (engine has them in memory)
     */
    public WorkflowRuntime updateOutput(long id, WfState wfState, long runStartedMillis, WfRuntimeMetricsSummary tokenSummary) {
        WorkflowRuntime updateOne = new WorkflowRuntime();
        updateOne.setId(id);
        ObjectNode ob = JsonUtil.createObjectNode();
        for (NodeIOData data : wfState.getOutput()) {
            ob.set(data.getName(), JsonUtil.classToJsonNode(data.getContent()));
        }
        updateOne.setOutput(ob);
        updateOne.setStatus(wfState.getProcessStatus());
        applyMetricsSnapshot(updateOne, runStartedMillis, tokenSummary);
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
     * @param runStartedMillis epoch millis when the run started, used for wall-clock duration
     * @param tokenSummary     pre-computed token totals from the caller
     */
    public WorkflowRuntime updateStatus(long id, int processStatus, String statusRemark, long runStartedMillis, WfRuntimeMetricsSummary tokenSummary) {
        WorkflowRuntime updateOne = new WorkflowRuntime();
        updateOne.setId(id);
        updateOne.setStatus(processStatus);
        updateOne.setStatusRemark(StringUtils.substring(statusRemark, 0, 250));
        applyMetricsSnapshot(updateOne, runStartedMillis, tokenSummary);
        if (baseMapper.updateById(updateOne) == 0) {
            log.error("Workflow runtime not found, id:{}", id);
            return null;
        }
        return updateOne;
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

    public boolean deleteAll(String wfUuid) {
        Workflow workflow = workflowService.getOrThrow(wfUuid);
        User user = ThreadContext.getCurrentUser();
        return ChainWrappers.lambdaUpdateChain(baseMapper)
                .eq(WorkflowRuntime::getWorkflowId, workflow.getId())
                .eq(!user.getIsAdmin(), WorkflowRuntime::getUserId, user.getId())
                .set(WorkflowRuntime::getIsDeleted, true)
                .update();
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
    private void applyMetricsSnapshot(WorkflowRuntime updateOne, long runStartedMillis, WfRuntimeMetricsSummary summary) {
        updateOne.setInputTokens(NumberUtil.saturatedCastToInt(summary.inputTokens()));
        updateOne.setOutputTokens(NumberUtil.saturatedCastToInt(summary.outputTokens()));
        long wallClockMs = Math.max(0, System.currentTimeMillis() - runStartedMillis);
        updateOne.setDuration(NumberUtil.saturatedCastToInt(wallClockMs));
    }

    /**
     * Copy the persisted flat snapshot onto the resp DTO. Null columns leave the resp fields null
     * so the UI hides them (e.g. still DOING, or legacy row).
     */
    private void fillMetricsFromEntity(WorkflowRuntime source, WfRuntimeResp target) {
        target.setInputTokens(source.getInputTokens() == null ? null : source.getInputTokens().longValue());
        target.setOutputTokens(source.getOutputTokens() == null ? null : source.getOutputTokens().longValue());
        target.setDuration(source.getDuration() == null ? null : source.getDuration().longValue());
    }

    public boolean softDelete(String uuid) {
        WorkflowRuntime workflowRuntime = PrivilegeUtil.checkAndGetByUuid(uuid, this.query(), ErrorEnum.A_WF_NOT_FOUND);
        return ChainWrappers.lambdaUpdateChain(baseMapper)
                .eq(WorkflowRuntime::getId, workflowRuntime.getId())
                .set(WorkflowRuntime::getIsDeleted, true)
                .update();
    }
}

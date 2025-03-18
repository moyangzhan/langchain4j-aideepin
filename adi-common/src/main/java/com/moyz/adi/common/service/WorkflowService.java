package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.baomidou.mybatisplus.extension.toolkit.ChainWrappers;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.workflow.WfEdgeReq;
import com.moyz.adi.common.dto.workflow.WfNodeDto;
import com.moyz.adi.common.dto.workflow.WorkflowResp;
import com.moyz.adi.common.dto.workflow.WorkflowUpdateReq;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.entity.Workflow;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.mapper.WorkflowMapper;
import com.moyz.adi.common.util.MPPageUtil;
import com.moyz.adi.common.util.PrivilegeUtil;
import com.moyz.adi.common.util.UuidUtil;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

@Slf4j
@Service
public class WorkflowService extends ServiceImpl<WorkflowMapper, Workflow> {

    @Resource
    private WorkflowNodeService workflowNodeService;

    @Resource
    private WorkflowEdgeService workflowEdgeService;

    @Resource
    private UserService userService;

    @Transactional
    public WorkflowResp add(String title, String remark) {
        String uuid = UuidUtil.createShort();
        Workflow one = new Workflow();
        one.setUuid(uuid);
        one.setTitle(title);
        one.setUserId(ThreadContext.getCurrentUserId());
        one.setRemark(remark);
        baseMapper.insert(one);

        workflowNodeService.createStartNode(one);
        return changeWorkflowToDTO(one);
    }

    public void updateBaseInfo(String wfUuid, String title, String remark) {
        if (StringUtils.isAnyBlank(wfUuid, title)) {
            throw new BaseException(ErrorEnum.A_PARAMS_ERROR);
        }
        ChainWrappers.lambdaUpdateChain(baseMapper)
                .eq(Workflow::getUuid, wfUuid)
                .set(Workflow::getTitle, title)
                .set(Workflow::getRemark, remark)
                .update();
    }

    @Transactional
    public void update(WorkflowUpdateReq req) {
        Workflow workflow = PrivilegeUtil.checkAndGetByUuid(req.getUuid(), this.query(), ErrorEnum.A_WF_NOT_FOUND);
        long workflowId = workflow.getId();
        workflowNodeService.createOrUpdateNodes(workflowId, req.getNodes());
        workflowEdgeService.createOrUpdateEdges(workflowId, req.getEdges());
        workflowNodeService.deleteNodes(workflowId, req.getDeleteNodes());
        workflowEdgeService.deleteEdges(workflowId, req.getDeleteEdges());
    }

    public Workflow getByUuid(String uuid) {
        return ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(Workflow::getUuid, uuid)
                .eq(Workflow::getIsDeleted, false)
                .last("limit 1")
                .one();
    }

    public WorkflowResp getDtoByUuid(String uuid) {
        Workflow wf = ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(Workflow::getUuid, uuid)
                .eq(Workflow::getIsDeleted, false)
                .last("limit 1")
                .one();
        return changeWorkflowToDTO(wf);
    }

    public Workflow getOrThrow(String uuid) {
        Workflow workflow = ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(Workflow::getUuid, uuid)
                .eq(Workflow::getIsDeleted, false)
                .last("limit 1")
                .one();
        if (null == workflow) {
            throw new BaseException(ErrorEnum.A_WF_NOT_FOUND);
        }
        return workflow;
    }

    public Page<WorkflowResp> searchMine(String keyword, Integer currentPage, Integer pageSize) {
        User user = ThreadContext.getCurrentUser();
        Page<Workflow> page = ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(Workflow::getIsDeleted, false)
                .like(StringUtils.isNotBlank(keyword), Workflow::getTitle, keyword)
                .eq(!user.getIsAdmin(), Workflow::getUserId, user.getId())
                .page(new Page<>(currentPage, pageSize));
        Page<WorkflowResp> result = new Page<>();
        List<Long> userIds = new ArrayList<>();
        MPPageUtil.convertToPage(page, result, WorkflowResp.class, (source, target) -> {
            fillNodesAndEdges(target);
            userIds.add(source.getUserId());
            return target;
        });
        fillUserInfos(userIds, result.getRecords());
        return result;
    }

    public Page<WorkflowResp> searchPublic(String keyword, Integer currentPage, Integer pageSize) {
        Page<Workflow> page = ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(Workflow::getIsDeleted, false)
                .eq(Workflow::getIsPublic, true)
                .like(StringUtils.isNotBlank(keyword), Workflow::getTitle, keyword)
                .page(new Page<>(currentPage, pageSize));
        Page<WorkflowResp> result = new Page<>();
        List<Long> userIds = new ArrayList<>();
        MPPageUtil.convertToPage(page, result, WorkflowResp.class, (source, target) -> {
            fillNodesAndEdges(target);
            userIds.add(source.getUserId());
            return target;
        });
        fillUserInfos(userIds, result.getRecords());
        return result;
    }

    private WorkflowResp changeWorkflowToDTO(Workflow workflow) {
        WorkflowResp workflowResp = new WorkflowResp();
        BeanUtils.copyProperties(workflow, workflowResp);

        fillNodesAndEdges(workflowResp);
        User user = userService.getById(workflow.getUserId());
        if (null != user) {
            workflowResp.setUserUuid(user.getUuid());
            workflowResp.setUserName(user.getName());
        }
        return workflowResp;
    }

    private void fillNodesAndEdges(WorkflowResp workflowResp) {
        List<WfNodeDto> nodes = workflowNodeService.listByWfId(workflowResp.getId());
        workflowResp.setNodes(nodes);
        List<WfEdgeReq> edges = workflowEdgeService.listByWfId(workflowResp.getId());
        workflowResp.setEdges(edges);
    }

    private void fillUserInfos(List<Long> userIds, List<WorkflowResp> resps) {
        Map<Long, User> users = userService.listByIds(userIds).stream()
                .collect(Collectors.toMap(User::getId, Function.identity(), (s, a) -> s));
        for (WorkflowResp workflowResp : resps) {
            User user = users.get(workflowResp.getUserId());
            if (null != user) {
                workflowResp.setUserUuid(user.getUuid());
                workflowResp.setUserName(user.getName());
            }
        }
    }
}

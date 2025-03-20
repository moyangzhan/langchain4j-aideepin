package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.baomidou.mybatisplus.extension.toolkit.ChainWrappers;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.cosntant.RedisKeyConstant;
import com.moyz.adi.common.dto.TmpNode;
import com.moyz.adi.common.dto.workflow.WfEdgeReq;
import com.moyz.adi.common.dto.workflow.WfNodeDto;
import com.moyz.adi.common.dto.workflow.WorkflowResp;
import com.moyz.adi.common.dto.workflow.WorkflowUpdateReq;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.entity.Workflow;
import com.moyz.adi.common.entity.WorkflowEdge;
import com.moyz.adi.common.entity.WorkflowNode;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.mapper.WorkflowMapper;
import com.moyz.adi.common.util.MPPageUtil;
import com.moyz.adi.common.util.PrivilegeUtil;
import com.moyz.adi.common.util.RedisTemplateUtil;
import com.moyz.adi.common.util.UuidUtil;
import com.moyz.adi.common.workflow.WfNodeInputConfig;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.moyz.adi.common.cosntant.RedisKeyConstant.DRAW_COMMENT_LIMIT_KEY;
import static com.moyz.adi.common.enums.ErrorEnum.A_OPT_TOO_FREQUENTLY;

@Slf4j
@Service
public class WorkflowService extends ServiceImpl<WorkflowMapper, Workflow> {

    @Lazy
    @Resource
    private WorkflowService self;

    @Resource
    private WorkflowNodeService workflowNodeService;

    @Resource
    private WorkflowEdgeService workflowEdgeService;

    @Resource
    private UserService userService;

    @Resource
    private RedisTemplateUtil redisTemplateUtil;

    @Transactional
    public WorkflowResp add(String title, String remark, Boolean isPublic) {
        String uuid = UuidUtil.createShort();
        Workflow one = new Workflow();
        one.setUuid(uuid);
        one.setTitle(title);
        one.setUserId(ThreadContext.getCurrentUserId());
        one.setRemark(remark);
        one.setIsEnable(true);
        baseMapper.insert(one);

        workflowNodeService.createStartNode(one);
        return changeWorkflowToDTO(one);
    }

    @Transactional
    public WorkflowResp copy(String wfUuid) {
        String redisKey = MessageFormat.format(RedisKeyConstant.WORKFLOW_COPY_DOING, ThreadContext.getCurrentUserId());
        if (!redisTemplateUtil.lock(redisKey, UuidUtil.createShort(), 10)) {
            throw new BaseException(A_OPT_TOO_FREQUENTLY);
        }

        Workflow sourceWorkflow = getOrThrow(wfUuid);
        Workflow newWorkflow = new Workflow();
        newWorkflow.setUuid(UuidUtil.createShort());
        newWorkflow.setTitle(sourceWorkflow.getTitle() + "-copy");
        newWorkflow.setUserId(ThreadContext.getCurrentUserId());
        newWorkflow.setRemark(sourceWorkflow.getRemark());
        newWorkflow.setIsEnable(true);
        baseMapper.insert(newWorkflow);

        List<WorkflowNode> newNodes = workflowNodeService.copyByWorkflowId(sourceWorkflow.getId(), newWorkflow.getId());
        List<WorkflowEdge> newEdges = workflowEdgeService.copyByWorkflowId(sourceWorkflow.getId(), newWorkflow.getId());

        //节点及边所包含的uuid替换成新的uuid
        List<TmpNode> tmpNodes = newNodes.stream().map(node -> {
            TmpNode tmpNode = new TmpNode();
            tmpNode.setId(node.getId());
            tmpNode.setUuid(UuidUtil.createShort());
            tmpNode.setOldUuid(node.getUuid());
            tmpNode.setInputConfig(node.getInputConfig());
            return tmpNode;
        }).toList();
        List<WorkflowEdge> updateEdges = newEdges.stream().map(edge -> {

            tmpNodes.forEach(tmp -> {
                String oldNodeUuid = tmp.getOldUuid();
                String newNodeUuid = tmp.getUuid();
                if (oldNodeUuid.equals(edge.getSourceNodeUuid())) {
                    edge.setSourceNodeUuid(newNodeUuid);
                } else if (oldNodeUuid.equals(edge.getSourceHandle())) {
                    edge.setSourceHandle(newNodeUuid);
                } else if (oldNodeUuid.equals(edge.getTargetNodeUuid())) {
                    edge.setTargetNodeUuid(newNodeUuid);
                }
            });

            WorkflowEdge forUpdateEdge = new WorkflowEdge();
            forUpdateEdge.setId(edge.getId());
            forUpdateEdge.setUuid(edge.getUuid());
            forUpdateEdge.setSourceNodeUuid(edge.getSourceNodeUuid());
            forUpdateEdge.setTargetNodeUuid(edge.getTargetNodeUuid());
            forUpdateEdge.setSourceHandle(edge.getSourceHandle());
            return forUpdateEdge;
        }).toList();
        List<WorkflowNode> updateNodes = tmpNodes.stream().map(tmpNode -> {
            String oldNodeUuid = tmpNode.getOldUuid();
            String newNodeUuid = tmpNode.getUuid();
            tmpNodes.forEach(tmp -> {
                tmp.getInputConfig().getRefInputs().forEach(refInput -> {
                    if (refInput.getNodeUuid().equals(oldNodeUuid)) {
                        refInput.setNodeUuid(newNodeUuid);
                    }
                });
            });
            WorkflowNode forUpdateNode = new WorkflowNode();
            forUpdateNode.setId(tmpNode.getId());
            forUpdateNode.setUuid(tmpNode.getUuid());
            forUpdateNode.setInputConfig(tmpNode.getInputConfig());
            return forUpdateNode;
        }).toList();
        workflowNodeService.updateBatchById(updateNodes);
        workflowEdgeService.updateBatchById(updateEdges);

        return changeWorkflowToDTO(newWorkflow);
    }

    public void setPublic(String wfUuid) {
        Workflow workflow = PrivilegeUtil.checkAndGetByUuid(wfUuid, this.query(), ErrorEnum.A_WF_NOT_FOUND);
        ChainWrappers.lambdaUpdateChain(baseMapper)
                .eq(Workflow::getId, workflow.getId())
                .set(Workflow::getIsPublic, true)
                .update();
    }

    public WorkflowResp updateBaseInfo(String wfUuid, String title, String remark, Boolean isPublic) {
        if (StringUtils.isAnyBlank(wfUuid, title)) {
            throw new BaseException(ErrorEnum.A_PARAMS_ERROR);
        }
        ChainWrappers.lambdaUpdateChain(baseMapper)
                .eq(Workflow::getUuid, wfUuid)
                .set(Workflow::getTitle, title)
                .set(Workflow::getRemark, remark)
                .set(null != isPublic, Workflow::getIsPublic, isPublic)
                .update();
        Workflow workflow = getOrThrow(wfUuid);
        return changeWorkflowToDTO(workflow);
    }

    @Transactional
    public WorkflowResp update(WorkflowUpdateReq req) {
        Workflow workflow = PrivilegeUtil.checkAndGetByUuid(req.getUuid(), this.query(), ErrorEnum.A_WF_NOT_FOUND);
        long workflowId = workflow.getId();
        workflowNodeService.createOrUpdateNodes(workflowId, req.getNodes());
        workflowEdgeService.createOrUpdateEdges(workflowId, req.getEdges());
        workflowNodeService.deleteNodes(workflowId, req.getDeleteNodes());
        workflowEdgeService.deleteEdges(workflowId, req.getDeleteEdges());

        Workflow workflow2 = getOrThrow(req.getUuid());
        return changeWorkflowToDTO(workflow2);
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

    public void softDelete(String uuid) {
        PrivilegeUtil.checkAndDelete(uuid, this.query(), ChainWrappers.updateChain(baseMapper), ErrorEnum.A_WF_NOT_FOUND);
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

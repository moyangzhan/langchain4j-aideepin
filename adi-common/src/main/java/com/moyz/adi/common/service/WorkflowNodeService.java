package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.baomidou.mybatisplus.extension.toolkit.ChainWrappers;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.moyz.adi.common.base.NodeInputConfigTypeHandler;
import com.moyz.adi.common.dto.workflow.WfNodeDto;
import com.moyz.adi.common.entity.Workflow;
import com.moyz.adi.common.entity.WorkflowComponent;
import com.moyz.adi.common.entity.WorkflowNode;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.mapper.WorkflowNodeMapper;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.util.MPPageUtil;
import com.moyz.adi.common.util.UuidUtil;
import com.moyz.adi.common.workflow.WfNodeInputConfig;
import com.moyz.adi.common.workflow.def.WfNodeIOText;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;

@Slf4j
@Service
public class WorkflowNodeService extends ServiceImpl<WorkflowNodeMapper, WorkflowNode> {

    @Lazy
    @Resource
    private WorkflowNodeService self;

    @Resource
    private WorkflowComponentService workflowComponentService;

    public WorkflowNode getStartNode(long workflowId) {
        return baseMapper.getStartNode(workflowId);
    }

    public List<WfNodeDto> listByWfId(long workflowId) {
        List<WorkflowNode> workflowNodeList = ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(WorkflowNode::getWorkflowId, workflowId)
                .eq(WorkflowNode::getIsDeleted, false)
                .list();
        return MPPageUtil.convertToList(workflowNodeList, WfNodeDto.class, (source, target) -> {
            target.setInputConfig((ObjectNode) JsonUtil.classToJsonNode(source.getInputConfig()));
            return target;
        });
    }

    public WorkflowNode getByUuid(String uuid) {
        return ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(WorkflowNode::getUuid, uuid)
                .eq(WorkflowNode::getIsDeleted, false)
                .last("limit 1")
                .one();
    }

    @Transactional
    public void createOrUpdateNodes(Long workflowId, List<WfNodeDto> nodes) {
        for (WfNodeDto node : nodes) {
            WorkflowNode newOrUpdate = new WorkflowNode();
            BeanUtils.copyProperties(node, newOrUpdate, "inputConfig");
            newOrUpdate.setInputConfig(NodeInputConfigTypeHandler.createNodeInputConfig(node.getInputConfig()));
            newOrUpdate.setWorkflowId(workflowId);

            WorkflowNode old = self.getByUuid(node.getUuid());
            if (null != old) {
                if (!old.getWorkflowId().equals(node.getWorkflowId())) {
                    log.error("节点不属于指定的工作流,保存失败,workflowId:{},old workflowId:{},new workflowId:{},node uuid:{},title:{}",
                            workflowId, old.getWorkflowId(), node.getWorkflowId(), node.getUuid(), node.getTitle());
                    throw new BaseException(ErrorEnum.A_PARAMS_ERROR);
                }
                log.info("更新节点,uuid:{},title:{}", node.getUuid(), node.getTitle());
            } else {
                log.info("新增节点,uuid:{},title:{}", node.getUuid(), node.getTitle());
                newOrUpdate.setId(null);
            }
            self.saveOrUpdate(newOrUpdate);
        }
    }

    @Transactional
    public void deleteNodes(Long workflowId, List<String> uuids) {
        if (CollectionUtils.isEmpty(uuids)) {
            return;
        }
        for (String uuid : uuids) {
            WorkflowNode old = self.getByUuid(uuid);
            if (null == old) {
                continue;
            }
            if (!old.getWorkflowId().equals(workflowId)) {
                log.error("节点不属于指定的工作流,删除失败,workflowId:{},node workflowId:{}", workflowId, workflowId);
                throw new BaseException(ErrorEnum.A_PARAMS_ERROR);
            }
            if (workflowComponentService.getStartComponent().getId().equals(old.getWorkflowComponentId())) {
                log.warn("开始节点不能删除,uuid:{}", old.getUuid());
                continue;
            }
            ChainWrappers.lambdaUpdateChain(baseMapper)
                    .eq(WorkflowNode::getWorkflowId, workflowId)
                    .eq(WorkflowNode::getUuid, uuid)
                    .set(WorkflowNode::getIsDeleted, true)
                    .update();
        }

    }

    /**
     * user_inputs:
     * [
     * {
     * "uuid": "12bc919774aa4e779d97e3dd9c836e11",
     * "name": "var_user_input",
     * "title": "用户输入",
     * "type": 1,
     * "required": true,
     * "max_length": 1000
     * }
     * ]
     *
     * @param workflow 工作流定义
     */
    public WorkflowNode createStartNode(Workflow workflow) {
        WfNodeIOText wfNodeIOText = WfNodeIOText.builder()
                .name("var_user_input")
                .title("用户输入")
                .required(false)
                .maxLength(1000)
                .build();
        WfNodeInputConfig nodeInputConfig = new WfNodeInputConfig();
        nodeInputConfig.setUserInputs(List.of(wfNodeIOText));
        nodeInputConfig.setRefInputs(new ArrayList<>());
        WorkflowComponent startComponent = workflowComponentService.getStartComponent();
        WorkflowNode node = new WorkflowNode();
        node.setWorkflowComponentId(startComponent.getId());
        node.setWorkflowId(workflow.getId());
        node.setRemark("用户输入");
        node.setUuid(UuidUtil.createShort());
        node.setTitle("开始");
        node.setInputConfig(nodeInputConfig);
        baseMapper.insert(node);
        return node;
    }
}

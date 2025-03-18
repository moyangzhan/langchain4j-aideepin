package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.baomidou.mybatisplus.extension.toolkit.ChainWrappers;
import com.moyz.adi.common.dto.workflow.WfEdgeReq;
import com.moyz.adi.common.entity.WorkflowEdge;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.mapper.WorkflowEdgeMapper;
import com.moyz.adi.common.util.MPPageUtil;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Slf4j
@Service
public class WorkflowEdgeService extends ServiceImpl<WorkflowEdgeMapper, WorkflowEdge> {

    @Lazy
    @Resource
    private WorkflowEdgeService self;

    public List<WfEdgeReq> listByWfId(long workflowId) {
        List<WorkflowEdge> edges = ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(WorkflowEdge::getWorkflowId, workflowId)
                .eq(WorkflowEdge::getIsDeleted, false)
                .list();
        return MPPageUtil.convertToList(edges, WfEdgeReq.class);
    }

    @Transactional
    public void createOrUpdateEdges(Long workflowId, List<WfEdgeReq> edges) {
        for (WfEdgeReq edge : edges) {
            WorkflowEdge newOne = new WorkflowEdge();
            BeanUtils.copyProperties(edge, newOne);
            newOne.setWorkflowId(workflowId);

            WorkflowEdge old = self.getByUuid(edge.getUuid());
            if (null != old) {
                if (!old.getWorkflowId().equals(edge.getWorkflowId())) {
                    log.error("该边不属于指定的工作流,保存失败,workflowId:{},old workflowId:{},new workflowId:{}", workflowId, old.getWorkflowId(), edge.getWorkflowId());
                    throw new BaseException(ErrorEnum.A_PARAMS_ERROR);
                }
                log.info("更新边,id:{},uuid:{},source:{},sourceHandle:{},target:{}",
                        edge.getId(), edge.getUuid(), edge.getSourceNodeUuid(), edge.getSourceHandle(), edge.getTargetNodeUuid());
            } else {
                newOne.setId(null);
                log.info("新增边,uuid:{},source:{},sourceHandle:{},target:{}",
                        edge.getUuid(), edge.getSourceNodeUuid(), edge.getSourceHandle(), edge.getTargetNodeUuid());
            }
            self.saveOrUpdate(newOne);
        }
    }

    @Transactional
    public void deleteEdges(Long workflowId, List<String> uuids) {
        if (CollectionUtils.isEmpty(uuids)) {
            return;
        }
        for (String uuid : uuids) {
            WorkflowEdge old = self.getByUuid(uuid);
            if (null != old && !old.getWorkflowId().equals(workflowId)) {
                log.error("该边不属于指定的工作流,删除失败,workflowId:{},node workflowId:{}", workflowId, workflowId);
                throw new BaseException(ErrorEnum.A_PARAMS_ERROR);
            }
            ChainWrappers.lambdaUpdateChain(baseMapper)
                    .eq(WorkflowEdge::getWorkflowId, workflowId)
                    .eq(WorkflowEdge::getUuid, uuid)
                    .set(WorkflowEdge::getIsDeleted, true)
                    .update();
        }
    }

    public WorkflowEdge getByUuid(String uuid) {
        return ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(WorkflowEdge::getUuid, uuid)
                .eq(WorkflowEdge::getIsDeleted, false)
                .last("limit 1")
                .one();
    }
}

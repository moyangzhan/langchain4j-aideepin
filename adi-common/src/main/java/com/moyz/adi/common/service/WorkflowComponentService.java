package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.baomidou.mybatisplus.extension.toolkit.ChainWrappers;
import com.moyz.adi.common.entity.WorkflowComponent;
import com.moyz.adi.common.mapper.WorkflowComponentMapper;
import com.moyz.adi.common.workflow.WfComponentNameEnum;
import lombok.extern.slf4j.Slf4j;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

import java.util.List;

import static com.moyz.adi.common.cosntant.RedisKeyConstant.WORKFLOW_COMPONENT_START_KEY;
import static com.moyz.adi.common.cosntant.RedisKeyConstant.WORKFLOW_KEY;

@Slf4j
@Service
public class WorkflowComponentService extends ServiceImpl<WorkflowComponentMapper, WorkflowComponent> {

    public List<WorkflowComponent> getAllEnable() {
        return ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(WorkflowComponent::getIsEnable, true)
                .orderByAsc(WorkflowComponent::getId)
                .list();
    }

    @Cacheable(cacheNames = WORKFLOW_KEY, key = WORKFLOW_COMPONENT_START_KEY)
    public WorkflowComponent getStartComponent() {
        return ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(WorkflowComponent::getName, WfComponentNameEnum.START.getName())
                .eq(WorkflowComponent::getIsEnable, true)
                .last("limit 1")
                .one();
    }
}

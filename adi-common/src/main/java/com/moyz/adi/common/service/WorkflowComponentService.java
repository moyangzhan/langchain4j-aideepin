package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.baomidou.mybatisplus.extension.toolkit.ChainWrappers;
import com.moyz.adi.common.entity.WorkflowComponent;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.mapper.WorkflowComponentMapper;
import com.moyz.adi.common.workflow.WfComponentNameEnum;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

import java.util.List;

import static com.moyz.adi.common.cosntant.RedisKeyConstant.*;

@Slf4j
@Service
public class WorkflowComponentService extends ServiceImpl<WorkflowComponentMapper, WorkflowComponent> {

    @Lazy
    @Resource
    private WorkflowComponentService self;

    @Cacheable(cacheNames = WORKFLOW_COMPONENTS)
    public List<WorkflowComponent> getAllEnable() {
        return ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(WorkflowComponent::getIsEnable, true)
                .orderByAsc(List.of(WorkflowComponent::getId, WorkflowComponent::getDisplayOrder))
                .list();
    }

    @Cacheable(cacheNames = WORKFLOW_COMPONENT_START_KEY)
    public WorkflowComponent getStartComponent() {
        return ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(WorkflowComponent::getName, WfComponentNameEnum.START.getName())
                .eq(WorkflowComponent::getIsEnable, true)
                .last("limit 1")
                .one();
    }

    @Cacheable(cacheNames = WORKFLOW_COMPONENT_KEY, condition = "#id>0", key = "#p0")
    public WorkflowComponent getComponent(Long id) {
        List<WorkflowComponent> components = self.getAllEnable();
        return components.stream()
                .filter(component -> component.getId().equals(id))
                .findFirst()
                .orElseThrow(() -> new BaseException(ErrorEnum.B_WF_NODE_DEFINITION_NOT_FOUND));
    }
}

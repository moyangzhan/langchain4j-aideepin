package com.moyz.adi.common.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.entity.WorkflowRuntime;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

@Mapper
public interface WorkflowRunMapper extends BaseMapper<WorkflowRuntime> {

    Page<WorkflowRuntime> pageByWfUuid(Page<WorkflowRuntime> page, @Param("wfUuid") String wfUuid, @Param("userId") Long userId);
}

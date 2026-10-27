package com.moyz.adi.common.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.entity.WorkflowRuntime;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.time.LocalDateTime;

/**
 * SQL 语句位于 resources/mapper/WorkflowInstanceMapper.xml（namespace=本接口）。
 * <p>
 * Statements live in resources/mapper/WorkflowInstanceMapper.xml (namespace = this interface).
 * </p>
 */
@Mapper
public interface WorkflowRunMapper extends BaseMapper<WorkflowRuntime> {

    Page<WorkflowRuntime> pageByWfUuid(Page<WorkflowRuntime> page, @Param("wfUuid") String wfUuid, @Param("userId") Long userId);

    /**
     * Load the runtime row WITH the checkpoint blob for resume
     * (the entity keeps checkpoint out of default SELECTs via select=false, so the resume path must query it explicitly).
     */
    WorkflowRuntime selectWithCheckpoint(@Param("uuid") String uuid);

    /**
     * Resume concurrency gate: conditional WAITING_INPUT -> DOING in one UPDATE —
     * the winner proceeds, duplicate submits are rejected atomically.
     */
    int casWaitingInputToRunning(@Param("uuid") String uuid);

    /**
     * Write output + terminal snapshot + checkpoint in ONE update; a null checkpoint explicitly
     * CLEARS the column.
     */
    int updateOutputWithCheckpoint(WorkflowRuntime runtime);

    /**
     * Terminal status update that always CLEARS the checkpoint column.
     */
    int updateStatusWithCheckpoint(WorkflowRuntime runtime);

    /**
     * Timeout expiry: fail runtimes stuck in WAITING_INPUT past the threshold and clear their checkpoint
     * (update_time marks the interruption moment).
     */
    int expireWaitingInput(@Param("threshold") LocalDateTime threshold, @Param("remark") String remark);
}

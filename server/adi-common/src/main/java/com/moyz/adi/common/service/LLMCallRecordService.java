package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.entity.LLMCallRecord;
import com.moyz.adi.common.mapper.LLMCallRecordMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.util.Collection;
import java.util.Collections;
import java.util.List;

/**
 * LLM 调用记录 Service | LLM call record service
 * <p>异步写入，避免阻塞主业务流程</p>
 * <p>Asynchronous write to avoid blocking the main business flow</p>
 */
@Slf4j
@Service
public class LLMCallRecordService extends ServiceImpl<LLMCallRecordMapper, LLMCallRecord> {

    /**
     * 异步保存 LLM 调用记录 | Save LLM call record asynchronously
     */
    @Async
    public void saveAsync(LLMCallRecord record) {
        save(record);
    }

    /**
     * 根据 sourceType 和 sourceId 列表查询 | Query by source type and source IDs
     */
    public List<LLMCallRecord> listBySource(Integer sourceType, Collection<Long> sourceIds) {
        if (sourceIds == null || sourceIds.isEmpty()) {
            return Collections.emptyList();
        }
        return this.lambdaQuery()
                .eq(LLMCallRecord::getSourceType, sourceType)
                .in(LLMCallRecord::getSourceId, sourceIds)
                .list();
    }
}

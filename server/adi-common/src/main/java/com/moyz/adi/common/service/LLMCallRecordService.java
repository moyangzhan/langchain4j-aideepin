package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.dto.LLMCallRecordDto;
import com.moyz.adi.common.dto.LLMCallRecordSearchReq;
import com.moyz.adi.common.entity.LLMCallRecord;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.mapper.LLMCallRecordMapper;
import com.moyz.adi.common.util.LocalDateTimeUtil;
import com.moyz.adi.common.util.MPPageUtil;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * LLM 调用记录 Service | LLM call record service
 * <p>异步写入，避免阻塞主业务流程</p>
 * <p>Asynchronous write to avoid blocking the main business flow</p>
 */
@Slf4j
@Service
public class LLMCallRecordService extends ServiceImpl<LLMCallRecordMapper, LLMCallRecord> {

    @Lazy
    @Resource
    private UserService userService;

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

    /**
     * 分页查询 LLM 调用记录 | Paginated query of LLM call records
     * <p>支持按用户名、模型名、模型平台、来源类型、请求时间范围过滤，并回填关联用户名</p>
     * <p>Supports filtering by user name, model name, model platform, source type and request time range,
     * and backfills the associated user name</p>
     *
     * @param req         搜索条件 | search criteria
     * @param currentPage 当前页 | current page
     * @param pageSize    每页大小 | page size
     * @return 分页结果 | page result
     */
    public Page<LLMCallRecordDto> search(LLMCallRecordSearchReq req, Integer currentPage, Integer pageSize) {
        LambdaQueryWrapper<LLMCallRecord> wrapper = new LambdaQueryWrapper<>();
        if (null != req.getSourceType()) {
            wrapper.eq(LLMCallRecord::getSourceType, req.getSourceType());
        }
        if (StringUtils.isNotBlank(req.getModelName())) {
            wrapper.like(LLMCallRecord::getModelName, req.getModelName());
        }
        if (StringUtils.isNotBlank(req.getModelPlatform())) {
            wrapper.eq(LLMCallRecord::getModelPlatform, req.getModelPlatform());
        }
        if (null != req.getRequestTime() && req.getRequestTime().length == 2) {
            wrapper.between(LLMCallRecord::getRequestTime,
                    LocalDateTimeUtil.parse(req.getRequestTime()[0]),
                    LocalDateTimeUtil.parse(req.getRequestTime()[1]));
        }
        if (StringUtils.isNotBlank(req.getUserName())) {
            List<Long> userIds = userService.lambdaQuery()
                    .like(User::getName, req.getUserName())
                    .list()
                    .stream()
                    .map(User::getId)
                    .collect(Collectors.toList());
            if (userIds.isEmpty()) {
                return new Page<>();
            }
            wrapper.in(LLMCallRecord::getUserId, userIds);
        }
        wrapper.eq(LLMCallRecord::getIsDeleted, false);
        wrapper.orderByDesc(LLMCallRecord::getRequestTime);
        Page<LLMCallRecord> page = baseMapper.selectPage(new Page<>(currentPage, pageSize), wrapper);

        List<Long> userIds = page.getRecords().stream()
                .map(LLMCallRecord::getUserId)
                .filter(java.util.Objects::nonNull)
                .distinct()
                .collect(Collectors.toList());
        Map<Long, String> idToName = new HashMap<>();
        if (!userIds.isEmpty()) {
            idToName = userService.lambdaQuery()
                    .in(User::getId, userIds)
                    .list()
                    .stream()
                    .collect(Collectors.toMap(User::getId, User::getName, (a, b) -> a));
        }
        Map<Long, String> finalIdToName = idToName;
        return MPPageUtil.convertToPage(page, new Page<>(), LLMCallRecordDto.class, (src, dto) -> {
            dto.setUserName(finalIdToName.get(src.getUserId()));
            return dto;
        });
    }
}

package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.baomidou.mybatisplus.extension.toolkit.ChainWrappers;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.AiSearchRecordResp;
import com.moyz.adi.common.dto.AiSearchResp;
import com.moyz.adi.common.dto.SearchEngineResp;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.entity.AiSearchRecord;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.mapper.AiSearchRecordMapper;
import com.moyz.adi.common.util.BizPager;
import com.moyz.adi.common.util.MPPageUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

import static com.moyz.adi.common.enums.ErrorEnum.A_DATA_NOT_FOUND;
import static com.moyz.adi.common.util.LocalCache.MODEL_ID_TO_OBJ;

/**
 * Ai search
 */
@Slf4j
@Service
public class AiSearchRecordService extends ServiceImpl<AiSearchRecordMapper, AiSearchRecord> {

    /**
     * List search records
     *
     * @param maxId   Anchor id
     * @param keyword user's question
     * @return
     */
    public AiSearchResp listByMaxId(Long maxId, String keyword) {
        LambdaQueryWrapper<AiSearchRecord> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(AiSearchRecord::getUserId, ThreadContext.getCurrentUserId());
        wrapper.eq(AiSearchRecord::getIsDeleted, false);
        if (StringUtils.isNotBlank(keyword)) {
            wrapper.like(AiSearchRecord::getQuestion, keyword);
        }
        AiSearchResp result = new AiSearchResp();
        BizPager.listByMaxId(maxId, wrapper, this, AiSearchRecord::getId, (recordList, minId) -> {
            List<AiSearchRecordResp> list = MPPageUtil.convertToList(recordList, AiSearchRecordResp.class);
            list.forEach(item -> {
                if (null == item.getSearchEngineResp()) {
                    SearchEngineResp searchEngineResp = new SearchEngineResp();
                    searchEngineResp.setItems(new ArrayList<>());
                    item.setSearchEngineResp(searchEngineResp);
                }
                AiModel aiModel = MODEL_ID_TO_OBJ.get(item.getAiModelId());
                item.setAiModelPlatform(null == aiModel ? "" : aiModel.getPlatform());
            });
            result.setRecords(list);
            result.setMinId(minId);
        });
        return result;
    }

    public boolean softDelete(String uuid) {
        if (Boolean.TRUE.equals(ThreadContext.getCurrentUser().getIsAdmin())) {
            return ChainWrappers.lambdaUpdateChain(baseMapper)
                    .eq(AiSearchRecord::getUuid, uuid)
                    .set(AiSearchRecord::getIsDeleted, true)
                    .update();
        }
        AiSearchRecord exist = ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(AiSearchRecord::getUuid, uuid)
                .eq(AiSearchRecord::getUserId, ThreadContext.getCurrentUserId())
                .one();
        if (null == exist) {
            throw new BaseException(A_DATA_NOT_FOUND);
        }
        return ChainWrappers.lambdaUpdateChain(baseMapper)
                .eq(AiSearchRecord::getId, exist.getId())
                .set(AiSearchRecord::getIsDeleted, true)
                .update();
    }

}

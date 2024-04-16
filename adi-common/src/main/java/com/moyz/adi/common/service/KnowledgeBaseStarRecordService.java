package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.baomidou.mybatisplus.extension.toolkit.ChainWrappers;
import com.moyz.adi.common.dto.KbStarInfoResp;
import com.moyz.adi.common.entity.KnowledgeBaseStarRecord;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.mapper.KnowledgeBaseStarRecordMapper;
import com.moyz.adi.common.util.MPPageUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

@Slf4j
@Service
public class KnowledgeBaseStarRecordService extends ServiceImpl<KnowledgeBaseStarRecordMapper, KnowledgeBaseStarRecord> {

    public boolean isStarred(Long userId, String kbUuid) {
        return ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(KnowledgeBaseStarRecord::getUserId, userId)
                .eq(KnowledgeBaseStarRecord::getKbUuid, kbUuid)
                .eq(KnowledgeBaseStarRecord::getIsDeleted, false)
                .exists();
    }

    public KnowledgeBaseStarRecord getRecord(long userId, String kbUuid){
        return ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(KnowledgeBaseStarRecord::getUserId, userId)
                .eq(KnowledgeBaseStarRecord::getKbUuid, kbUuid)
                .oneOpt()
                .orElse(null);
    }

    public Page<KbStarInfoResp> listStarInfo(User user, int currentPage, int pageSize) {
        LambdaQueryWrapper<KnowledgeBaseStarRecord> wrapper = new LambdaQueryWrapper();
        wrapper.eq(KnowledgeBaseStarRecord::getIsDeleted, false);
        wrapper.eq(KnowledgeBaseStarRecord::getUserId, user.getId());
        wrapper.orderByDesc(KnowledgeBaseStarRecord::getId);
        Page<KnowledgeBaseStarRecord> list = baseMapper.selectPage(new Page<>(currentPage, pageSize), wrapper);

        Page<KbStarInfoResp> result = new Page<>();
        return MPPageUtil.convertToPage(list, result, KbStarInfoResp.class);
    }

}

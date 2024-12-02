package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.baomidou.mybatisplus.extension.toolkit.ChainWrappers;
import com.moyz.adi.common.dto.KbStarInfoResp;
import com.moyz.adi.common.entity.KnowledgeBaseStar;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.mapper.KnowledgeBaseStarMapper;
import com.moyz.adi.common.util.MPPageUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

@Slf4j
@Service
public class KnowledgeBaseStarService extends ServiceImpl<KnowledgeBaseStarMapper, KnowledgeBaseStar> {

    public boolean isStarred(Long userId, String kbUuid) {
        return ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(KnowledgeBaseStar::getUserId, userId)
                .eq(KnowledgeBaseStar::getKbUuid, kbUuid)
                .eq(KnowledgeBaseStar::getIsDeleted, false)
                .exists();
    }

    public KnowledgeBaseStar getRecord(long userId, String kbUuid){
        return ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(KnowledgeBaseStar::getUserId, userId)
                .eq(KnowledgeBaseStar::getKbUuid, kbUuid)
                .oneOpt()
                .orElse(null);
    }

    public Page<KbStarInfoResp> listStarInfo(User user, int currentPage, int pageSize) {
        LambdaQueryWrapper<KnowledgeBaseStar> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(KnowledgeBaseStar::getIsDeleted, false);
        wrapper.eq(KnowledgeBaseStar::getUserId, user.getId());
        wrapper.orderByDesc(KnowledgeBaseStar::getId);
        Page<KnowledgeBaseStar> list = baseMapper.selectPage(new Page<>(currentPage, pageSize), wrapper);

        Page<KbStarInfoResp> result = new Page<>();
        return MPPageUtil.convertToPage(list, result, KbStarInfoResp.class);
    }

}

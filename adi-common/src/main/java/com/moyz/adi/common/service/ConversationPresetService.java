package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.ConvPresetAddReq;
import com.moyz.adi.common.dto.ConvPresetEditReq;
import com.moyz.adi.common.entity.ConversationPreset;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.mapper.ConversationPresetMapper;
import com.moyz.adi.common.util.UuidUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

@Slf4j
@Service
public class ConversationPresetService extends ServiceImpl<ConversationPresetMapper, ConversationPreset> {
    public Page<ConversationPreset> search(String keyword, int currentPage, int pageSize) {
        return this.lambdaQuery()
                .eq(ConversationPreset::getIsDeleted, false)
                .like(StringUtils.isBlank(keyword) ? false : true, ConversationPreset::getTitle, keyword)
                .orderByDesc(ConversationPreset::getUpdateTime)
                .page(new Page<>(currentPage, pageSize));
    }

    public ConversationPreset addOne(ConvPresetAddReq presetAddReq) {
        if (StringUtils.isAnyBlank(presetAddReq.getTitle(), presetAddReq.getRemark())) {
            throw new BaseException(ErrorEnum.A_PARAMS_ERROR);
        }
        ConversationPreset newOne = new ConversationPreset();
        newOne.setUuid(UuidUtil.createShort());
        newOne.setTitle(presetAddReq.getTitle());
        newOne.setRemark(presetAddReq.getRemark());
        newOne.setAiSystemMessage(presetAddReq.getAiSystemMessage());
        this.save(newOne);
        return newOne;
    }

    public boolean edit(String uuid, ConvPresetEditReq editReq) {
        if (StringUtils.isAnyBlank(uuid, editReq.getTitle(), editReq.getRemark())) {
            throw new BaseException(ErrorEnum.A_PARAMS_ERROR);
        }
        if (!ThreadContext.getCurrentUser().getIsAdmin()) {
            throw new BaseException(ErrorEnum.A_USER_NOT_AUTH);
        }
        return this.lambdaUpdate()
                .eq(ConversationPreset::getUuid, uuid)
                .set(ConversationPreset::getTitle, editReq.getTitle())
                .set(ConversationPreset::getRemark, editReq.getRemark())
                .set(ConversationPreset::getAiSystemMessage, editReq.getAiSystemMessage())
                .update();
    }

    public boolean softDel(String uuid) {
        if (!ThreadContext.getCurrentUser().getIsAdmin()) {
            throw new BaseException(ErrorEnum.A_USER_NOT_AUTH);
        }
        return this.lambdaUpdate().eq(ConversationPreset::getUuid, uuid).set(ConversationPreset::getIsDeleted, true).update();
    }
}

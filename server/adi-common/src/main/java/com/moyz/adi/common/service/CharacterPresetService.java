package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.CharacterPresetAddReq;
import com.moyz.adi.common.dto.CharacterPresetEditReq;
import com.moyz.adi.common.entity.CharacterPreset;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.mapper.CharacterPresetMapper;
import com.moyz.adi.common.util.UuidUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

@Slf4j
@Service
public class CharacterPresetService extends ServiceImpl<CharacterPresetMapper, CharacterPreset> {
    public Page<CharacterPreset> search(String keyword, int currentPage, int pageSize) {
        return this.lambdaQuery()
                .eq(CharacterPreset::getIsDeleted, false)
                .like(!StringUtils.isBlank(keyword), CharacterPreset::getTitle, keyword)
                .orderByDesc(CharacterPreset::getUpdateTime)
                .page(new Page<>(currentPage, pageSize));
    }

    public CharacterPreset addOne(CharacterPresetAddReq presetAddReq) {
        if (StringUtils.isAnyBlank(presetAddReq.getTitle(), presetAddReq.getRemark())) {
            throw new BaseException(ErrorEnum.A_PARAMS_ERROR);
        }
        CharacterPreset newOne = new CharacterPreset();
        newOne.setUuid(UuidUtil.createShort());
        newOne.setTitle(presetAddReq.getTitle());
        newOne.setRemark(presetAddReq.getRemark());
        newOne.setAiSystemMessage(presetAddReq.getAiSystemMessage());
        newOne.setKbTitle(presetAddReq.getKbTitle());
        newOne.setType(presetAddReq.getType());
        this.save(newOne);
        return newOne;
    }

    public boolean edit(String uuid, CharacterPresetEditReq editReq) {
        if (StringUtils.isAnyBlank(uuid, editReq.getTitle(), editReq.getRemark())) {
            throw new BaseException(ErrorEnum.A_PARAMS_ERROR);
        }
        if (Boolean.FALSE.equals(ThreadContext.getCurrentUser().getIsAdmin())) {
            throw new BaseException(ErrorEnum.A_USER_NOT_AUTH);
        }
        return this.lambdaUpdate()
                .eq(CharacterPreset::getUuid, uuid)
                .set(CharacterPreset::getTitle, editReq.getTitle())
                .set(CharacterPreset::getRemark, editReq.getRemark())
                .set(CharacterPreset::getAiSystemMessage, editReq.getAiSystemMessage())
                .set(CharacterPreset::getKbTitle, editReq.getKbTitle())
                .set(CharacterPreset::getType, editReq.getType())
                .update();
    }

    public boolean softDel(String uuid) {
        if (Boolean.FALSE.equals(ThreadContext.getCurrentUser().getIsAdmin())) {
            throw new BaseException(ErrorEnum.A_USER_NOT_AUTH);
        }
        return this.lambdaUpdate().eq(CharacterPreset::getUuid, uuid).set(CharacterPreset::getIsDeleted, true).update();
    }
}

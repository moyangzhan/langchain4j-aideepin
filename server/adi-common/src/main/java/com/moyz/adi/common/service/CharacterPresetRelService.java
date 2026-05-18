package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.dto.CharacterPresetRelDto;
import com.moyz.adi.common.entity.CharacterPresetRel;
import com.moyz.adi.common.mapper.CharacterPresetRelMapper;
import com.moyz.adi.common.util.MPPageUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
public class CharacterPresetRelService extends ServiceImpl<CharacterPresetRelMapper, CharacterPresetRel> {

    public List<CharacterPresetRelDto> listByUser(Long userId, Integer limit) {
        List<CharacterPresetRel> list = this.lambdaQuery()
                .eq(CharacterPresetRel::getUserId, userId)
                .eq(CharacterPresetRel::getIsDeleted, false)
                .last("limit " + limit)
                .list();
        return MPPageUtil.convertToList(list, CharacterPresetRelDto.class);
    }

    public boolean softDelBy(Long userId, Long characterId) {
        return this.lambdaUpdate()
                .eq(CharacterPresetRel::getUserId, userId)
                .eq(CharacterPresetRel::getUserCharacterId, characterId)
                .set(CharacterPresetRel::getIsDeleted, true)
                .update();
    }
}

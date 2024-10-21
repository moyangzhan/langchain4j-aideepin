package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.dto.ConvPresetRelDto;
import com.moyz.adi.common.entity.ConversationPresetRel;
import com.moyz.adi.common.mapper.ConversationPresetRelMapper;
import com.moyz.adi.common.util.MPPageUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
public class ConversationPresetRelService extends ServiceImpl<ConversationPresetRelMapper, ConversationPresetRel> {

    public List<ConvPresetRelDto> listByUser(Long userId, Integer limit) {
        List<ConversationPresetRel> list = this.lambdaQuery()
                .eq(ConversationPresetRel::getUserId, userId)
                .eq(ConversationPresetRel::getIsDeleted, false)
                .last("limit " + limit)
                .list();
        return MPPageUtil.convertToList(list, ConvPresetRelDto.class);
    }
}

package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.spring.service.impl.ServiceImpl;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.RefEmbeddingDto;
import com.moyz.adi.common.entity.CharacterMessageRefEmbedding;
import com.moyz.adi.common.mapper.CharacterMessageRefEmbeddingMapper;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.List;

@Slf4j
@Service
public class CharacterMessageRefEmbeddingService extends ServiceImpl<CharacterMessageRefEmbeddingMapper, CharacterMessageRefEmbedding> {

    /**
     * 段内容唯一事实源在关系表：text→段内容；问题→答案内容；子块→父段内容
     */
    @Resource
    private DocumentSegmentService documentSegmentService;

    public List<RefEmbeddingDto> listRefEmbeddings(String msgUuid) {
        List<CharacterMessageRefEmbedding> recordReferences = this.getBaseMapper().listByMsgUuid(msgUuid, ThreadContext.getCurrentUserId());
        if (CollectionUtils.isEmpty(recordReferences)) {
            return Collections.emptyList();
        }
        List<String> embeddingIds = recordReferences.stream().map(CharacterMessageRefEmbedding::getEmbeddingId).toList();
        if (CollectionUtils.isEmpty(embeddingIds)) {
            return Collections.emptyList();
        }
        return documentSegmentService.listRefTextsByEmbeddingIds(embeddingIds);
    }

}

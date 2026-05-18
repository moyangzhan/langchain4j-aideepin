package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.dto.KbItemEmbeddingDto;
import com.moyz.adi.common.dto.RefEmbeddingDto;
import com.moyz.adi.common.entity.CharacterMessageRefEmbedding;
import com.moyz.adi.common.mapper.CharacterMessageRefEmbeddingMapper;
import com.moyz.adi.common.service.embedding.ICharacterMemoryEmbeddingService;
import com.moyz.adi.common.util.EmbeddingUtil;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.List;

@Slf4j
@Service
public class CharacterMessageRefEmbeddingService extends ServiceImpl<CharacterMessageRefEmbeddingMapper, CharacterMessageRefEmbedding> {

    @Resource
    private ICharacterMemoryEmbeddingService characterMemoryEmbeddingService;

    public List<RefEmbeddingDto> listRefEmbeddings(String msgUuid) {
        List<CharacterMessageRefEmbedding> recordReferences = this.getBaseMapper().listByMsgUuid(msgUuid);
        if (CollectionUtils.isEmpty(recordReferences)) {
            return Collections.emptyList();
        }
        List<String> embeddingIds = recordReferences.stream().map(CharacterMessageRefEmbedding::getEmbeddingId).toList();
        if (CollectionUtils.isEmpty(embeddingIds)) {
            return Collections.emptyList();
        }
        List<KbItemEmbeddingDto> embeddings = characterMemoryEmbeddingService.listByEmbeddingIds(embeddingIds);
        return EmbeddingUtil.itemToRefEmbeddingDto(embeddings);
    }

}

package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.dto.KbItemEmbeddingDto;
import com.moyz.adi.common.dto.RefEmbeddingDto;
import com.moyz.adi.common.entity.ConversationMessageRefMemoryEmbedding;
import com.moyz.adi.common.mapper.ConversationMessageRefMemoryEmbeddingMapper;
import com.moyz.adi.common.service.embedding.IConvMemoryEmbeddingService;
import com.moyz.adi.common.util.EmbeddingUtil;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.List;

@Slf4j
@Service
public class ConversationMessageRefMemoryEmbeddingService extends ServiceImpl<ConversationMessageRefMemoryEmbeddingMapper, ConversationMessageRefMemoryEmbedding> {

    @Resource
    private IConvMemoryEmbeddingService convMemoryEmbeddingService;

    public List<RefEmbeddingDto> listRefEmbeddings(String uuid) {
        List<ConversationMessageRefMemoryEmbedding> recordReferences = this.getBaseMapper().listByMsgUuid(uuid);
        if (CollectionUtils.isEmpty(recordReferences)) {
            return Collections.emptyList();
        }
        List<String> embeddingIds = recordReferences.stream().map(ConversationMessageRefMemoryEmbedding::getEmbeddingId).toList();
        if (CollectionUtils.isEmpty(embeddingIds)) {
            return Collections.emptyList();
        }
        List<KbItemEmbeddingDto> embeddings = convMemoryEmbeddingService.listByEmbeddingIds(embeddingIds);
        return EmbeddingUtil.itemToRefEmbeddingDto(embeddings);
    }

}

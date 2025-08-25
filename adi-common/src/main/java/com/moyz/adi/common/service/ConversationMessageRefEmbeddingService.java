package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.dto.KbItemEmbeddingDto;
import com.moyz.adi.common.dto.RefEmbeddingDto;
import com.moyz.adi.common.entity.ConversationMessageRefEmbedding;
import com.moyz.adi.common.mapper.ConversationMessageRefEmbeddingMapper;
import com.moyz.adi.common.service.embedding.IEmbeddingService;
import com.moyz.adi.common.util.EmbeddingUtil;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.List;

@Slf4j
@Service
public class ConversationMessageRefEmbeddingService extends ServiceImpl<ConversationMessageRefEmbeddingMapper, ConversationMessageRefEmbedding> {

    @Resource
    private IEmbeddingService iEmbeddingService;

    public List<RefEmbeddingDto> listRefEmbeddings(String msgUuid) {
        List<ConversationMessageRefEmbedding> recordReferences = this.getBaseMapper().listByMsgUuid(msgUuid);
        if (CollectionUtils.isEmpty(recordReferences)) {
            return Collections.emptyList();
        }
        List<String> embeddingIds = recordReferences.stream().map(ConversationMessageRefEmbedding::getEmbeddingId).toList();
        if (CollectionUtils.isEmpty(embeddingIds)) {
            return Collections.emptyList();
        }
        List<KbItemEmbeddingDto> embeddings = iEmbeddingService.listByEmbeddingIds(embeddingIds);
        return EmbeddingUtil.itemToRefEmbeddingDto(embeddings);
    }

}

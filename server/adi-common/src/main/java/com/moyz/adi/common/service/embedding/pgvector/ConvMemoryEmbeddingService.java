package com.moyz.adi.common.service.embedding.pgvector;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.dto.KbItemEmbeddingDto;
import com.moyz.adi.common.entity.ConversationMemoryEmbedding;
import com.moyz.adi.common.mapper.ConversationMemoryEmbeddingMapper;
import com.moyz.adi.common.service.embedding.IConvMemoryEmbeddingService;
import com.moyz.adi.common.util.MPPageUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.UUID;

@Slf4j
@Service
@ConditionalOnProperty(value = "adi.vector-database", havingValue = "pgvector")
public class ConvMemoryEmbeddingService extends ServiceImpl<ConversationMemoryEmbeddingMapper, ConversationMemoryEmbedding> implements IConvMemoryEmbeddingService {

    @Override
    public List<KbItemEmbeddingDto> listByEmbeddingIds(List<String> embeddingIds) {
        LambdaQueryWrapper<ConversationMemoryEmbedding> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper.in(ConversationMemoryEmbedding::getEmbeddingId, embeddingIds.stream().map(UUID::fromString).toList());
        List<ConversationMemoryEmbedding> embeddingList = baseMapper.selectList(lambdaQueryWrapper);
        return MPPageUtil.convertToList(embeddingList, KbItemEmbeddingDto.class, (s, t) -> {
            t.setEmbedding(s.getEmbedding().toArray());
            return t;
        });
    }

}

package com.moyz.adi.common.service.embedding;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.dto.KbItemEmbeddingDto;
import com.moyz.adi.common.entity.KnowledgeBaseEmbedding;
import com.moyz.adi.common.mapper.KnowledgeBaseEmbeddingMapper;
import com.moyz.adi.common.util.AdiPropertiesUtil;
import com.moyz.adi.common.util.MPPageUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.UUID;

@Slf4j
@Service
@ConditionalOnProperty(value = "adi.vector-database", havingValue = "pgvector")
public class KnowledgeBaseEmbeddingService extends ServiceImpl<KnowledgeBaseEmbeddingMapper, KnowledgeBaseEmbedding> implements IEmbeddingService {

    @Override
    public List<KbItemEmbeddingDto> listByEmbeddingIds(List<String> embeddingIds) {
        LambdaQueryWrapper<KnowledgeBaseEmbedding> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper.in(KnowledgeBaseEmbedding::getEmbeddingId, embeddingIds.stream().map(UUID::fromString).toList());
        List<KnowledgeBaseEmbedding> embeddingList = baseMapper.selectList(lambdaQueryWrapper);
        return MPPageUtil.convertToList(embeddingList, KbItemEmbeddingDto.class, (s, t) -> {
            t.setEmbedding(s.getEmbedding().toArray());
            return t;
        });
    }

    @Override
    public Page<KbItemEmbeddingDto> listByItemUuid(String kbItemUuid, int currentPage, int pageSize) {
        Page<KnowledgeBaseEmbedding> sourcePage = baseMapper.selectByItemUuid(new Page<>(currentPage, pageSize), kbItemUuid, AdiPropertiesUtil.EMBEDDING_TABLE_SUFFIX);
        Page<KbItemEmbeddingDto> result = new Page<>();
        MPPageUtil.convertToPage(sourcePage, result, KbItemEmbeddingDto.class, (source, target) -> {
            target.setEmbedding(source.getEmbedding().toArray());
            return target;
        });
        return result;
    }

    /**
     * 删除{kbItemUuid}这个知识库条目的向量
     *
     * @param kbItemUuid 知识库条目uuid
     * @return
     */
    @Override
    public boolean deleteByItemUuid(String kbItemUuid) {
        return baseMapper.deleteByItemUuid(kbItemUuid, AdiPropertiesUtil.EMBEDDING_TABLE_SUFFIX);
    }

    public Integer countByKbUuid(String kbUuid) {
        return baseMapper.countByKbUuid(kbUuid, AdiPropertiesUtil.EMBEDDING_TABLE_SUFFIX);
    }
}

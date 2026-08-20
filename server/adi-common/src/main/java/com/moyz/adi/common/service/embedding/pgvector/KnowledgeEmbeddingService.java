package com.moyz.adi.common.service.embedding.pgvector;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.entity.KnowledgeBaseEmbedding;
import com.moyz.adi.common.mapper.KnowledgeBaseEmbeddingMapper;
import com.moyz.adi.common.service.embedding.IKnowledgeEmbeddingService;
import com.moyz.adi.common.util.AdiPropertiesUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
@ConditionalOnProperty(value = "adi.vector-database", havingValue = "pgvector")
public class KnowledgeEmbeddingService extends ServiceImpl<KnowledgeBaseEmbeddingMapper, KnowledgeBaseEmbedding> implements IKnowledgeEmbeddingService {

    /**
     * 删除{kbItemUuid}这个文档的向量（按 metadata kb_item_uuid 过滤）
     *
     * @param kbItemUuid 文档uuid
     */
    @Override
    public boolean deleteByItemUuid(String kbItemUuid) {
        return baseMapper.deleteByItemUuid(kbItemUuid, AdiPropertiesUtil.EMBEDDING_TABLE_SUFFIX);
    }

    @Override
    public boolean deleteByIds(List<String> embeddingIds) {
        if (CollectionUtils.isEmpty(embeddingIds)) {
            return true;
        }
        return baseMapper.deleteByIds(embeddingIds, AdiPropertiesUtil.EMBEDDING_TABLE_SUFFIX);
    }
}

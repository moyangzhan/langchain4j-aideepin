package com.moyz.adi.common.service.embedding.neo4j;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.rag.neo4j.AdiNeo4jEmbeddingStore;
import com.moyz.adi.common.service.embedding.IKnowledgeEmbeddingService;
import dev.langchain4j.data.segment.TextSegment;
import dev.langchain4j.store.embedding.EmbeddingStore;
import dev.langchain4j.store.embedding.filter.comparison.IsEqualTo;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Service;

import java.util.List;


@Slf4j
@Service
@ConditionalOnProperty(value = "adi.vector-database", havingValue = "neo4j")
public class KnowledgeEmbeddingService implements IKnowledgeEmbeddingService {

    @Resource
    @Qualifier("kbEmbeddingStore")
    private EmbeddingStore<TextSegment> embeddingStore;

    @Override
    public boolean deleteByItemUuid(String kbItemUuid) {
        embeddingStore.removeAll(new IsEqualTo(AdiConstant.MetadataKey.KB_ITEM_UUID, kbItemUuid));
        return true;
    }

    @Override
    public boolean deleteByIds(List<String> embeddingIds) {
        if (CollectionUtils.isEmpty(embeddingIds)) {
            return true;
        }
        embeddingStore.removeAll(embeddingIds);
        return true;
    }
}

package com.moyz.adi.common.service.embedding.neo4j;

import com.moyz.adi.common.dto.KbItemEmbeddingDto;
import com.moyz.adi.common.rag.neo4j.AdiNeo4jEmbeddingStore;
import com.moyz.adi.common.service.embedding.IConvMemoryEmbeddingService;
import dev.langchain4j.data.segment.TextSegment;
import dev.langchain4j.store.embedding.EmbeddingMatch;
import dev.langchain4j.store.embedding.EmbeddingSearchResult;
import dev.langchain4j.store.embedding.EmbeddingStore;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;


@Slf4j
@Service
@ConditionalOnProperty(value = "adi.vector-database", havingValue = "neo4j")
public class ConvMemoryEmbeddingService implements IConvMemoryEmbeddingService {

    @Resource
    @Qualifier("convMemoryEmbeddingStore")
    private EmbeddingStore<TextSegment> embeddingStore;

    @Override
    public List<KbItemEmbeddingDto> listByEmbeddingIds(List<String> embeddingIds) {
        if (embeddingIds.isEmpty()) {
            log.warn("listByMemoryEmbeddingIds embeddingIds is empty");
            return new ArrayList<>();
        }
        EmbeddingSearchResult<TextSegment> searchResult = ((AdiNeo4jEmbeddingStore) embeddingStore).searchByIds(embeddingIds);
        List<KbItemEmbeddingDto> result = new ArrayList<>();
        for (EmbeddingMatch<TextSegment> embeddingMatch : searchResult.matches()) {
            result.add(
                    KbItemEmbeddingDto
                            .builder()
                            .embeddingId(embeddingMatch.embeddingId())
                            .embedding(embeddingMatch.embedding().vector())
                            .text(embeddingMatch.embedded().text())
                            .build()
            );
        }
        return result;
    }

}

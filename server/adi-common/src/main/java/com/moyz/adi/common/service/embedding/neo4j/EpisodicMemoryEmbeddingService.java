package com.moyz.adi.common.service.embedding.neo4j;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.dto.KbItemEmbeddingDto;
import com.moyz.adi.common.rag.neo4j.AdiNeo4jEmbeddingStore;
import com.moyz.adi.common.service.embedding.IEpisodicMemoryEmbeddingService;
import dev.langchain4j.data.segment.TextSegment;
import dev.langchain4j.store.embedding.EmbeddingMatch;
import dev.langchain4j.store.embedding.EmbeddingSearchResult;
import dev.langchain4j.store.embedding.EmbeddingStore;
import dev.langchain4j.store.embedding.filter.comparison.IsEqualTo;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

import static com.moyz.adi.common.cosntant.AdiConstant.MetadataKey.CHARACTER_ID;
import static com.moyz.adi.common.cosntant.AdiConstant.MetadataKey.CREATED_AT;

/**
 * Neo4j implementation of {@link IEpisodicMemoryEmbeddingService}. Reverse-lookup
 * via the dedicated episodic memory store. Mirrors the semantic
 * {@link CharacterMemoryEmbeddingService}.
 */
@Slf4j
@Service
@ConditionalOnProperty(value = "adi.vector-database", havingValue = "neo4j")
public class EpisodicMemoryEmbeddingService implements IEpisodicMemoryEmbeddingService {

    private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

    @Resource
    @Qualifier("episodicMemoryEmbeddingStore")
    private EmbeddingStore<TextSegment> embeddingStore;

    @Override
    public List<KbItemEmbeddingDto> listByEmbeddingIds(List<String> embeddingIds) {
        if (embeddingIds.isEmpty()) {
            log.warn("listByEmbeddingIds embeddingIds is empty");
            return new ArrayList<>();
        }
        EmbeddingSearchResult<TextSegment> searchResult = ((AdiNeo4jEmbeddingStore) embeddingStore).searchByIds(embeddingIds);
        return toDtos(searchResult);
    }

    @Override
    public List<KbItemEmbeddingDto> listRecentByCharacter(Long characterId, int limit) {
        EmbeddingSearchResult<TextSegment> searchResult = ((AdiNeo4jEmbeddingStore) embeddingStore)
                .searchByMetadataOrdered(new IsEqualTo(CHARACTER_ID, characterId), CREATED_AT, limit);
        return toDtos(searchResult);
    }

    private List<KbItemEmbeddingDto> toDtos(EmbeddingSearchResult<TextSegment> searchResult) {
        List<KbItemEmbeddingDto> result = new ArrayList<>();
        for (EmbeddingMatch<TextSegment> embeddingMatch : searchResult.matches()) {
            result.add(
                    KbItemEmbeddingDto
                            .builder()
                            .embeddingId(embeddingMatch.embeddingId())
                            .embedding(embeddingMatch.embedding() != null ? embeddingMatch.embedding().vector() : null)
                            .text(embeddingMatch.embedded().text())
                            .metadata(OBJECT_MAPPER.valueToTree(embeddingMatch.embedded().metadata().toMap()))
                            .build()
            );
        }
        return result;
    }
}

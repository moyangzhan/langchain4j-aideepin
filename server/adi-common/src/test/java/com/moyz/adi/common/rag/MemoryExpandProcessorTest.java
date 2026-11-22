package com.moyz.adi.common.rag;

import com.moyz.adi.common.entity.CharacterMemoryEpisodic;
import com.moyz.adi.common.entity.CharacterMemorySemantic;
import com.moyz.adi.common.service.CharacterMemoryEpisodicService;
import com.moyz.adi.common.service.CharacterMemorySemanticService;
import dev.langchain4j.data.document.Metadata;
import dev.langchain4j.data.embedding.Embedding;
import dev.langchain4j.data.segment.TextSegment;
import dev.langchain4j.rag.content.Content;
import dev.langchain4j.rag.query.Query;
import dev.langchain4j.store.embedding.EmbeddingMatch;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * 记忆命中回表处理器契约钉子：命中内容从关系行取（向量 text 只是占位）、保留原命中 metadata 供 scoping/引用、无关系行或软删行的命中丢弃且不写分数表，处置与知识库通道一致。
 * <p>
 * Pins for the memory hydration processors: content comes from the relational row (vector text is only a placeholder), the hit's metadata is preserved for scoping/refs, and hits without an alive relational row are dropped without entering the score map, same handling as the KB channel.
 */
class MemoryExpandProcessorTest {

    private CharacterMemorySemanticService semanticService;
    private CharacterMemoryEpisodicService episodicService;

    @BeforeEach
    void setUp() {
        semanticService = mock(CharacterMemorySemanticService.class);
        episodicService = mock(CharacterMemoryEpisodicService.class);
    }

    @Test
    void semanticHydratesContentFromRelationalRowAndKeepsMetadata() {
        String uuid = "0193a0b0c0d0e0f0a0b0c0d0e0f0a0b0";
        CharacterMemorySemantic row = new CharacterMemorySemantic();
        row.setUuid(uuid);
        row.setContent("用户住在杭州");
        Map<String, CharacterMemorySemantic> byUuid = Map.of(uuid, row);
        when(semanticService.mapByUuids(anyList())).thenReturn(byUuid);

        SemanticMemoryExpandProcessor processor = new SemanticMemoryExpandProcessor();
        ReflectionTestUtils.setField(processor, "characterMemorySemanticService", semanticService);

        Map<String, Double> embeddingToScore = new HashMap<>();
        List<Content> result = processor.process(Query.from("q"),
                List.of(match(canonical(uuid), "stale vector text")), embeddingToScore);

        assertEquals(1, result.size());
        assertEquals("用户住在杭州", result.get(0).textSegment().text());
        assertEquals(7, result.get(0).textSegment().metadata().getInteger("conv_id"));
        assertEquals(0.9, embeddingToScore.get(canonical(uuid)));
    }

    @Test
    void semanticDropsHitsWithoutRelationalRow() {
        when(semanticService.mapByUuids(anyList())).thenReturn(Map.of());

        SemanticMemoryExpandProcessor processor = new SemanticMemoryExpandProcessor();
        ReflectionTestUtils.setField(processor, "characterMemorySemanticService", semanticService);

        Map<String, Double> embeddingToScore = new HashMap<>();
        List<Content> result = processor.process(Query.from("q"),
                List.of(match(canonical("0193a0b0c0d0e0f0a0b0c0d0e0f0a0b0"), "text")), embeddingToScore);

        assertTrue(result.isEmpty());
        assertTrue(embeddingToScore.isEmpty());
    }

    @Test
    void episodicHydratesContentFromRelationalRow() {
        String uuid = "0193a0b0c0d0e0f0a0b0c0d0e0f0a0b1";
        CharacterMemoryEpisodic row = new CharacterMemoryEpisodic();
        row.setUuid(uuid);
        row.setContent("用户去了北京");
        row.setCreateTime(LocalDateTime.of(2026, 11, 3, 9, 0));
        when(episodicService.mapByUuids(anyList())).thenReturn(Map.of(uuid, row));

        EpisodicMemoryExpandProcessor processor = new EpisodicMemoryExpandProcessor();
        ReflectionTestUtils.setField(processor, "characterMemoryEpisodicService", episodicService);

        Map<String, Double> embeddingToScore = new HashMap<>();
        List<Content> result = processor.process(Query.from("q"),
                List.of(match(canonical(uuid), "stale vector text")), embeddingToScore);

        assertEquals(1, result.size());
        assertEquals("用户去了北京", result.get(0).textSegment().text());
        assertEquals("2026-11-03", result.get(0).textSegment().metadata().getString("create_time"));
        assertEquals(1, embeddingToScore.size());
    }

    @Test
    void episodicLearnedDateOverwritesVectorLegacyCreateTime() {
        String uuid = "0193a0b0c0d0e0f0a0b0c0d0e0f0a0b2";
        CharacterMemoryEpisodic row = new CharacterMemoryEpisodic();
        row.setUuid(uuid);
        row.setContent("用户买了梨子");
        row.setCreateTime(LocalDateTime.of(2026, 11, 12, 10, 30));
        when(episodicService.mapByUuids(anyList())).thenReturn(Map.of(uuid, row));

        EpisodicMemoryExpandProcessor processor = new EpisodicMemoryExpandProcessor();
        ReflectionTestUtils.setField(processor, "characterMemoryEpisodicService", episodicService);

        Map<String, Object> legacyMeta = new HashMap<>(Map.of("conv_id", 7, "create_time", "2026-10-01 09:15:00"));
        EmbeddingMatch<TextSegment> legacyHit = new EmbeddingMatch<>(0.9, canonical(uuid),
                new Embedding(new float[]{0.1f}),
                TextSegment.from("stale vector text", new Metadata(legacyMeta)));

        List<Content> result = processor.process(Query.from("q"), List.of(legacyHit), new HashMap<>());

        assertEquals("2026-11-12", result.get(0).textSegment().metadata().getString("create_time"));
    }

    @Test
    void emptyMatchesReturnEmptyList() {
        SemanticMemoryExpandProcessor processor = new SemanticMemoryExpandProcessor();
        ReflectionTestUtils.setField(processor, "characterMemorySemanticService", semanticService);

        assertTrue(processor.process(Query.from("q"), List.of(), new HashMap<>()).isEmpty());
        assertTrue(processor.process(Query.from("q"), null, new HashMap<>()).isEmpty());
    }

    private static EmbeddingMatch<TextSegment> match(String embeddingId, String vectorText) {
        return new EmbeddingMatch<>(0.9, embeddingId, new Embedding(new float[]{0.1f}),
                TextSegment.from(vectorText, new Metadata(Map.of("conv_id", 7))));
    }

    private static String canonical(String shortUuid) {
        return new StringBuilder(36)
                .append(shortUuid, 0, 8).append('-')
                .append(shortUuid, 8, 12).append('-')
                .append(shortUuid, 12, 16).append('-')
                .append(shortUuid, 16, 20).append('-')
                .append(shortUuid, 20, 32)
                .toString();
    }
}

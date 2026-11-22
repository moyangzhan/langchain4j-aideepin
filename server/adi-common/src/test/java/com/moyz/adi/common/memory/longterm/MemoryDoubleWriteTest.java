package com.moyz.adi.common.memory.longterm;

import com.moyz.adi.common.config.AdiProperties;
import com.moyz.adi.common.entity.CharacterMemoryEpisodic;
import com.moyz.adi.common.entity.CharacterMemorySemantic;
import com.moyz.adi.common.memory.vo.BatchActionMemories;
import com.moyz.adi.common.memory.vo.ExtractedEpisodicEvent;
import com.moyz.adi.common.service.CharacterMemoryEpisodicService;
import com.moyz.adi.common.service.CharacterMemorySemanticService;
import com.moyz.adi.common.util.UuidUtil;
import dev.langchain4j.data.document.Metadata;
import dev.langchain4j.data.embedding.Embedding;
import dev.langchain4j.data.segment.TextSegment;
import dev.langchain4j.model.embedding.EmbeddingModel;
import dev.langchain4j.model.output.Response;
import dev.langchain4j.store.embedding.EmbeddingMatch;
import dev.langchain4j.store.embedding.EmbeddingSearchResult;
import dev.langchain4j.store.embedding.EmbeddingStore;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.InOrder;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 记忆双写契约钉子：① 关系行先于向量索引写入（episodic 批量、semantic ADD）；② 向量行主键 =
 * 关系行 uuid 的 canonical 36 位形；③ semantic UPDATE 同 uuid 重嵌新文本；④ DELETE 先移向量行、
 * 后软删关系行；⑤ episodic 结构化字段（事件码/重要性收敛/-1 哨兵）落关系行。
 * <p>
 * Pins for the memory double-write contract: relational rows precede vector-index
 * writes (episodic batch, semantic ADD); the vector row id equals the relational
 * row uuid in canonical form; semantic UPDATE re-embeds under the same uuid;
 * DELETE removes the vector row before soft-deleting the relational row; episodic
 * structured fields land on the relational row.
 */
class MemoryDoubleWriteTest {

    private EmbeddingModel embeddingModel;
    private EmbeddingStore<TextSegment> semanticStore;
    private EmbeddingStore<TextSegment> episodicStore;
    private CharacterMemorySemanticService semanticRelational;
    private CharacterMemoryEpisodicService episodicRelational;

    @BeforeEach
    void setUp() {
        embeddingModel = mock(EmbeddingModel.class);
        when(embeddingModel.embed(anyString())).thenReturn(Response.from(new Embedding(new float[]{0.1f})));
        semanticStore = mock(EmbeddingStore.class);
        episodicStore = mock(EmbeddingStore.class);
        semanticRelational = mock(CharacterMemorySemanticService.class);
        episodicRelational = mock(CharacterMemoryEpisodicService.class);
    }

    private EpisodicMemoryService newEpisodicService() {
        EpisodicMemoryService service = new EpisodicMemoryService();
        ReflectionTestUtils.setField(service, "episodicEmbeddingStore", episodicStore);
        ReflectionTestUtils.setField(service, "embeddingModel", embeddingModel);
        ReflectionTestUtils.setField(service, "characterMemoryEpisodicService", episodicRelational);
        return service;
    }

    private SemanticMemoryService newSemanticService() {
        SemanticMemoryService service = new SemanticMemoryService();
        ReflectionTestUtils.setField(service, "semanticEmbeddingStore", semanticStore);
        ReflectionTestUtils.setField(service, "embeddingModel", embeddingModel);
        ReflectionTestUtils.setField(service, "characterMemorySemanticService", semanticRelational);
        ReflectionTestUtils.setField(service, "adiProperties", new AdiProperties());
        return service;
    }

    @Test
    void episodicBatchAddWritesRelationalRowsBeforeVectorIndex() {
        ExtractedEpisodicEvent event = new ExtractedEpisodicEvent();
        event.setSummary("用户开始了新工作");
        event.setEventType("WORK");
        event.setImportance(9);

        EpisodicMemoryService service = newEpisodicService();
        service.batchAdd(7L, 42L, 100L, List.of(event), false);

        InOrder inOrder = inOrder(episodicRelational, episodicStore);
        inOrder.verify(episodicRelational).saveBatch(anyList());
        inOrder.verify(episodicStore).addAll(anyList(), anyList(), anyList());
    }

    @Test
    void episodicRowsCarryNormalizedFieldsAndCanonicalVectorId() {
        ExtractedEpisodicEvent event = new ExtractedEpisodicEvent();
        event.setSummary("用户开始学习法语");
        event.setEventType("LEARNING");
        event.setImportance(9);
        ExtractedEpisodicEvent lowEdge = new ExtractedEpisodicEvent();
        lowEdge.setSummary("用户随口提了一句");
        lowEdge.setEventType("GENERAL");
        lowEdge.setImportance(0);

        newEpisodicService().batchAdd(7L, 42L, null, List.of(event, lowEdge), false);

        ArgumentCaptor<Collection<CharacterMemoryEpisodic>> rowsCaptor = ArgumentCaptor.forClass(Collection.class);
        verify(episodicRelational).saveBatch(rowsCaptor.capture());
        List<CharacterMemoryEpisodic> rows = List.copyOf(rowsCaptor.getValue());
        assertEquals(2, rows.size());
        CharacterMemoryEpisodic row = rows.get(0);
        assertEquals(32, row.getUuid().length());
        assertEquals(7L, row.getCharacterId());
        assertEquals("用户开始学习法语", row.getContent());
        assertEquals("learning", row.getEventType());
        assertEquals(5, row.getImportance());
        assertEquals(-1L, row.getSourceMsgId());
        assertEquals(42L, row.getUserId());
        assertEquals(1, rows.get(1).getImportance());

        @SuppressWarnings("unchecked")
        ArgumentCaptor<List<String>> idsCaptor = ArgumentCaptor.forClass(List.class);
        verify(episodicStore).addAll(idsCaptor.capture(), anyList(), anyList());
        assertEquals(UuidUtil.toCanonical(row.getUuid()), idsCaptor.getValue().get(0));
    }

    @Test
    void episodicBlankEventsWriteNothing() {
        ExtractedEpisodicEvent blank = new ExtractedEpisodicEvent();
        blank.setSummary(" ");

        newEpisodicService().batchAdd(7L, 42L, 100L, List.of(blank), false);

        verify(episodicRelational, never()).saveBatch(anyList());
        verify(episodicStore, never()).addAll(anyList(), anyList(), anyList());
    }

    @Test
    void semanticAddInsertsRelationalRowThenEmbedsWholeFact() {
        BatchActionMemories.BatchActionMemory action = new BatchActionMemories.BatchActionMemory();
        action.setEvent("ADD");
        action.setFact("ignored");
        action.setText("用户住在杭州");

        newSemanticService().applyActions(batchOf(action), 7L, 42L, emptyCandidates());

        InOrder inOrder = inOrder(semanticRelational, semanticStore);
        inOrder.verify(semanticRelational).save(any(CharacterMemorySemantic.class));
        inOrder.verify(semanticStore).addAll(anyList(), anyList(), anyList());
        verify(embeddingModel).embed("用户住在杭州");
    }

    @Test
    void semanticUpdateReembedsNewTextUnderSameId() {
        String memoryId = UuidUtil.createShort();
        BatchActionMemories.BatchActionMemory action = new BatchActionMemories.BatchActionMemory();
        action.setEvent("UPDATE");
        action.setFact("f");
        action.setId("0");
        action.setText("用户搬到了上海");

        newSemanticService().applyActions(batchOf(action), 7L, 42L,
                candidatesWith("f", "0", UuidUtil.toCanonical(memoryId)));

        InOrder inOrder = inOrder(semanticRelational, semanticStore);
        inOrder.verify(semanticRelational).upsertContent(memoryId, 7L, "用户搬到了上海", 42L);
        inOrder.verify(semanticStore).remove(UuidUtil.toCanonical(memoryId));
        inOrder.verify(semanticStore).addAll(anyList(), anyList(), anyList());
        verify(embeddingModel).embed("用户搬到了上海");
        @SuppressWarnings("unchecked")
        ArgumentCaptor<List<String>> idsCaptor = ArgumentCaptor.forClass(List.class);
        verify(semanticStore).addAll(idsCaptor.capture(), anyList(), anyList());
        assertEquals(UuidUtil.toCanonical(memoryId), idsCaptor.getValue().get(0));
    }

    @Test
    void semanticDeleteRemovesVectorFirstThenSoftDeletesRelational() {
        String memoryId = UuidUtil.createShort();
        BatchActionMemories.BatchActionMemory action = new BatchActionMemories.BatchActionMemory();
        action.setEvent("DELETE");
        action.setFact("f");
        action.setId("0");

        newSemanticService().applyActions(batchOf(action), 7L, 42L,
                candidatesWith("f", "0", UuidUtil.toCanonical(memoryId)));

        InOrder inOrder = inOrder(semanticStore, semanticRelational);
        inOrder.verify(semanticStore).remove(UuidUtil.toCanonical(memoryId));
        inOrder.verify(semanticRelational).softDeleteByUuid(memoryId);
    }

    @Test
    void semanticUpdateWithUnknownTmpIdTouchesNothing() {
        BatchActionMemories.BatchActionMemory action = new BatchActionMemories.BatchActionMemory();
        action.setEvent("DELETE");
        action.setFact("f");
        action.setId("9");

        newSemanticService().applyActions(batchOf(action), 7L, 42L, candidatesWith("f", "0", UuidUtil.toCanonical(UuidUtil.createShort())));

        verify(semanticStore, never()).remove(anyString());
        verify(semanticRelational, never()).softDeleteByUuid(anyString());
    }

    @Test
    void addToVectorCarriesScopingAndSnapshotMetadataAndUuidPlaceholderText() {
        newSemanticService().applyActions(batchOf(addAction("用户养了一只猫")), 7L, 42L, emptyCandidates());

        @SuppressWarnings("unchecked")
        ArgumentCaptor<List<String>> idsCaptor = ArgumentCaptor.forClass(List.class);
        @SuppressWarnings("unchecked")
        ArgumentCaptor<List<TextSegment>> segmentsCaptor = ArgumentCaptor.forClass(List.class);
        verify(semanticStore).addAll(idsCaptor.capture(), anyList(), segmentsCaptor.capture());
        Metadata metadata = segmentsCaptor.getValue().get(0).metadata();
        assertEquals(2, metadata.toMap().size());
        assertEquals(7, metadata.getInteger("conv_id"));
        assertTrue(metadata.getString("create_time").matches("\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}"));
        assertEquals(idsCaptor.getValue().get(0), segmentsCaptor.getValue().get(0).text());
        assertEquals(36, segmentsCaptor.getValue().get(0).text().length());
    }

    @Test
    void searchMergeCandidatesHydratesPromptTextFromRelationalRowsAndSkipsOrphans() {
        String knownUuid = UuidUtil.createShort();
        String orphanUuid = UuidUtil.createShort();
        TextSegment placeholder = TextSegment.from("stale vector text", new Metadata(Map.of()));
        List<EmbeddingMatch<TextSegment>> matches = List.of(
                new EmbeddingMatch<>(0.95, UuidUtil.toCanonical(knownUuid), new Embedding(new float[]{0.1f}), placeholder),
                new EmbeddingMatch<>(0.90, UuidUtil.toCanonical(orphanUuid), new Embedding(new float[]{0.1f}), placeholder));
        when(semanticStore.search(any())).thenReturn(new EmbeddingSearchResult<>(matches));
        CharacterMemorySemantic row = new CharacterMemorySemantic();
        row.setUuid(knownUuid);
        row.setContent("关系行内容");
        when(semanticRelational.mapByUuids(anyList())).thenReturn(Map.of(knownUuid, row));

        MergeCandidates candidates =
                newSemanticService().searchMergeCandidates(List.of("新事实"), 7L);

        List<Map<String, String>> prompt = candidates.factToOldMemoriesForPrompt.get("新事实");
        assertEquals(1, prompt.size());
        assertEquals("关系行内容", prompt.get(0).get("text"));
        assertEquals(UuidUtil.toCanonical(knownUuid),
                candidates.factToTmpIdToEmbeddingId.get("新事实").get("0"));
    }

    private static BatchActionMemories batchOf(BatchActionMemories.BatchActionMemory action) {
        BatchActionMemories batch = new BatchActionMemories();
        batch.setActions(List.of(action));
        return batch;
    }

    private static BatchActionMemories.BatchActionMemory addAction(String text) {
        BatchActionMemories.BatchActionMemory action = new BatchActionMemories.BatchActionMemory();
        action.setEvent("ADD");
        action.setFact("ignored");
        action.setText(text);
        return action;
    }

    private static MergeCandidates emptyCandidates() {
        return new MergeCandidates(java.util.Map.of(), java.util.Map.of());
    }

    private static MergeCandidates candidatesWith(String fact, String tmpId, String embeddingId) {
        return new MergeCandidates(
                java.util.Map.of(fact, List.of(java.util.Map.of("id", tmpId, "text", "old"))),
                java.util.Map.of(fact, java.util.Map.of(tmpId, embeddingId)));
    }

    @Test
    void episodicVectorSegmentCarriesScopingAndSnapshotMetadataAndUuidPlaceholderText() {
        ExtractedEpisodicEvent event = new ExtractedEpisodicEvent();
        event.setSummary("用户去了北京");
        event.setEventType("TRAVEL");
        event.setImportance(4);

        newEpisodicService().batchAdd(7L, 42L, 100L, List.of(event), false);

        @SuppressWarnings("unchecked")
        ArgumentCaptor<List<String>> idsCaptor = ArgumentCaptor.forClass(List.class);
        @SuppressWarnings("unchecked")
        ArgumentCaptor<List<TextSegment>> segmentsCaptor = ArgumentCaptor.forClass(List.class);
        verify(episodicStore).addAll(idsCaptor.capture(), anyList(), segmentsCaptor.capture());
        Metadata metadata = segmentsCaptor.getValue().get(0).metadata();
        assertEquals(2, metadata.toMap().size());
        assertEquals(7, metadata.getInteger("conv_id"));
        assertTrue(metadata.getString("create_time").matches("\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}"));
        assertEquals(idsCaptor.getValue().get(0), segmentsCaptor.getValue().get(0).text());
        assertEquals(36, segmentsCaptor.getValue().get(0).text().length());
    }
}

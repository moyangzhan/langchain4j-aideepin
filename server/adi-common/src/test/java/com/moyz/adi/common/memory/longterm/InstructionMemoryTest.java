package com.moyz.adi.common.memory.longterm;

import com.moyz.adi.common.entity.CharacterMemoryInstruction;
import com.moyz.adi.common.memory.vo.BatchActionMemories;
import com.moyz.adi.common.memory.vo.InstructionCompressionDirectives;
import com.moyz.adi.common.service.CharacterMemoryInstructionService;
import com.moyz.adi.common.util.UuidUtil;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * instruction 通道契约钉子：① 无向量层——ADD 只插关系行（携带来源消息 id），DELETE 只软删，
 * UPDATE 只 upsert 内容；② 合并候选=全量生效规则集直给（每条新规则看到同一份全集、
 * tmpId 映射到规则 uuid）；③ 压缩指令按 uuid 编辑——合并改写保留行+删除被并入行、
 * 未知 uuid 忽略。
 * <p>
 * Pins for the instruction channel: no vector layer — ADD only inserts the relational
 * row (with the source message id), DELETE only soft-deletes, UPDATE only upserts
 * content; merge candidates are the FULL alive rule set (every new rule sees the same
 * list, tmpIds map to rule uuids); compression directives edit by uuid — merges
 * rewrite the kept row and drop the folded ones, unknown uuids are ignored.
 */
class InstructionMemoryTest {

    private CharacterMemoryInstructionService instructionRelational;

    @BeforeEach
    void setUp() {
        instructionRelational = mock(CharacterMemoryInstructionService.class);
    }

    private InstructionMemoryService newService() {
        InstructionMemoryService service = new InstructionMemoryService();
        ReflectionTestUtils.setField(service, "characterMemoryInstructionService", instructionRelational);
        return service;
    }

    @Test
    void addInsertsRelationalRowWithSourceMsgId() {
        BatchActionMemories.BatchActionMemory action = new BatchActionMemories.BatchActionMemory();
        action.setEvent("ADD");
        action.setFact("ignored");
        action.setText("回答代码问题时先给结论");

        newService().applyActions(batchOf(action), 7L, 42L, 100L, emptyCandidates());

        ArgumentCaptor<CharacterMemoryInstruction> rowCaptor = ArgumentCaptor.forClass(CharacterMemoryInstruction.class);
        verify(instructionRelational).save(rowCaptor.capture());
        CharacterMemoryInstruction row = rowCaptor.getValue();
        assertEquals(32, row.getUuid().length());
        assertEquals(7L, row.getCharacterId());
        assertEquals("回答代码问题时先给结论", row.getContent());
        assertEquals(100L, row.getSourceMsgId());
        assertEquals(42L, row.getUserId());
    }

    @Test
    void deleteOnlySoftDeletesRelationalRow() {
        String uuid = UuidUtil.createShort();
        BatchActionMemories.BatchActionMemory action = new BatchActionMemories.BatchActionMemory();
        action.setEvent("DELETE");
        action.setFact("r");
        action.setId("0");

        newService().applyActions(batchOf(action), 7L, 42L, 100L,
                candidatesWith("r", "0", uuid));

        verify(instructionRelational).softDeleteByUuid(uuid);
    }

    @Test
    void updateOnlyUpsertsContent() {
        String uuid = UuidUtil.createShort();
        BatchActionMemories.BatchActionMemory action = new BatchActionMemories.BatchActionMemory();
        action.setEvent("UPDATE");
        action.setFact("r");
        action.setId("0");
        action.setText("回答保持简洁");

        newService().applyActions(batchOf(action), 7L, 42L, 100L,
                candidatesWith("r", "0", uuid));

        verify(instructionRelational).upsertContent(uuid, 7L, "回答保持简洁", 42L);
    }

    @Test
    void mergeCandidatesAreTheFullAliveRuleSet() {
        CharacterMemoryInstruction first = row("a".repeat(32), "回答要简洁");
        CharacterMemoryInstruction second = row("b".repeat(32), "用中文回答");
        when(instructionRelational.listAllAliveByCharacter(7L)).thenReturn(List.of(first, second));

        MergeCandidates candidates =
                newService().searchMergeCandidates(List.of("回答简洁点"), 7L);

        List<Map<String, String>> oldMemories = candidates.factToOldMemoriesForPrompt.get("回答简洁点");
        assertEquals(2, oldMemories.size());
        assertEquals("回答要简洁", oldMemories.get(0).get("text"));
        Map<String, String> tmpIdToUuid = candidates.factToTmpIdToEmbeddingId.get("回答简洁点");
        assertEquals("a".repeat(32), tmpIdToUuid.get("0"));
        assertEquals("b".repeat(32), tmpIdToUuid.get("1"));
    }

    @Test
    void compressionMergeRewritesKeptRowAndDropsFolded() {
        String keepUuid = UuidUtil.createShort();
        String dropUuid = UuidUtil.createShort();
        CharacterMemoryInstruction keepRow = row(keepUuid, "回答要简洁");
        CharacterMemoryInstruction dropRow = row(dropUuid, "Keep answers concise");

        InstructionCompressionDirectives directives = new InstructionCompressionDirectives();
        InstructionCompressionDirectives.Merge merge = new InstructionCompressionDirectives.Merge();
        merge.setKeep(keepUuid);
        merge.setText("回答保持简洁（中英皆然）");
        merge.setDrop(List.of(dropUuid, keepUuid));
        directives.setMerges(List.of(merge));

        newService().applyCompressionDirectives(directives, List.of(keepRow, dropRow));

        verify(instructionRelational).upsertContent(keepUuid, 7L, "回答保持简洁（中英皆然）", 42L);
        verify(instructionRelational).softDeleteByUuid(dropUuid);
        verify(instructionRelational, never()).softDeleteByUuid(keepUuid);
    }

    @Test
    void compressionDirectivesIgnoreUnknownUuids() {
        InstructionCompressionDirectives directives = new InstructionCompressionDirectives();
        directives.setDeletes(List.of("deadbeefdeadbeefdeadbeefdeadbeef"));

        newService().applyCompressionDirectives(directives, List.of(row(UuidUtil.createShort(), "规则")));

        verify(instructionRelational, never()).softDeleteByUuid(org.mockito.ArgumentMatchers.anyString());
    }

    private static CharacterMemoryInstruction row(String uuid, String content) {
        return row(uuid, 7L, content);
    }

    private static CharacterMemoryInstruction row(String uuid, Long characterId, String content) {
        CharacterMemoryInstruction row = new CharacterMemoryInstruction();
        row.setUuid(uuid);
        row.setCharacterId(characterId);
        row.setUserId(42L);
        row.setContent(content);
        return row;
    }

    private static BatchActionMemories batchOf(BatchActionMemories.BatchActionMemory action) {
        BatchActionMemories batch = new BatchActionMemories();
        batch.setActions(List.of(action));
        return batch;
    }

    private static MergeCandidates emptyCandidates() {
        return new MergeCandidates(Map.of(), Map.of());
    }

    private static MergeCandidates candidatesWith(String fact, String tmpId, String uuid) {
        return new MergeCandidates(
                Map.of(fact, List.of(Map.of("id", tmpId, "text", "old"))),
                Map.of(fact, Map.of(tmpId, uuid)));
    }
}

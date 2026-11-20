package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.spring.service.impl.ServiceImpl;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.RefEmbeddingDto;
import com.moyz.adi.common.entity.CharacterMemoryEpisodic;
import com.moyz.adi.common.entity.CharacterMemoryInstruction;
import com.moyz.adi.common.entity.CharacterMemorySemantic;
import com.moyz.adi.common.entity.CharacterMessageRefMemoryEmbedding;
import com.moyz.adi.common.enums.MemoryType;
import com.moyz.adi.common.mapper.CharacterMessageRefMemoryEmbeddingMapper;
import com.moyz.adi.common.util.LocalDateTimeUtil;
import com.moyz.adi.common.util.UuidUtil;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;

@Slf4j
@Service
public class CharacterMessageRefMemoryEmbeddingService extends ServiceImpl<CharacterMessageRefMemoryEmbeddingMapper, CharacterMessageRefMemoryEmbedding> {

    @Resource
    private CharacterMemorySemanticService characterMemorySemanticService;

    @Resource
    private CharacterMemoryEpisodicService characterMemoryEpisodicService;

    @Resource
    private CharacterMemoryInstructionService characterMemoryInstructionService;

    /**
     * List memory references attached to a message, enriched with memory-type metadata
     * so the frontend can group semantic / episodic / instruction hits in the "Memory" popup.
     * <p>
     * Each ref row carries a {@code memory_type} routing code; rows are bucketed by
     * code and each bucket is resolved against its relational memory table by uuid in
     * a single batch lookup. Content and structured fields come from the relational
     * rows; refs whose memory row is gone (deleted) are skipped.
     * <p>
     * 列出消息引用的记忆。每条 ref 行带 memory_type 路由码，按 enum 分桶后各自回对应的
     * 关系记忆表按 uuid 一次性反查；内容与结构化字段来自关系行，记忆行已不存在的 ref 跳过。
     */
    public List<RefEmbeddingDto> listRefEmbeddings(String uuid) {
        List<CharacterMessageRefMemoryEmbedding> recordReferences = this.getBaseMapper().listByMsgUuid(uuid, ThreadContext.getCurrentUserId());
        if (CollectionUtils.isEmpty(recordReferences)) {
            return Collections.emptyList();
        }

        Map<MemoryType, List<CharacterMessageRefMemoryEmbedding>> byType = new EnumMap<>(MemoryType.class);
        for (CharacterMessageRefMemoryEmbedding ref : recordReferences) {
            if (ref.getEmbeddingId() == null || ref.getEmbeddingId().isEmpty()) {
                continue;
            }
            byType.computeIfAbsent(ref.getMemoryType(), k -> new ArrayList<>()).add(ref);
        }
        if (byType.isEmpty()) {
            return Collections.emptyList();
        }

        Map<String, CharacterMemorySemantic> semanticByUuid = Collections.emptyMap();
        if (byType.containsKey(MemoryType.SEMANTIC)) {
            List<String> uuids = uuidsOf(byType.get(MemoryType.SEMANTIC));
            semanticByUuid = characterMemorySemanticService.mapByUuids(uuids);
        }
        Map<String, CharacterMemoryEpisodic> episodicByUuid = Collections.emptyMap();
        if (byType.containsKey(MemoryType.EPISODIC)) {
            List<String> uuids = uuidsOf(byType.get(MemoryType.EPISODIC));
            episodicByUuid = characterMemoryEpisodicService.mapByUuids(uuids);
        }
        Map<String, CharacterMemoryInstruction> instructionByUuid = Collections.emptyMap();
        if (byType.containsKey(MemoryType.INSTRUCTION)) {
            List<String> uuids = uuidsOf(byType.get(MemoryType.INSTRUCTION));
            instructionByUuid = characterMemoryInstructionService.mapByUuids(uuids);
        }

        List<RefEmbeddingDto> dtos = new ArrayList<>();
        for (Map.Entry<MemoryType, List<CharacterMessageRefMemoryEmbedding>> entry : byType.entrySet()) {
            for (CharacterMessageRefMemoryEmbedding ref : entry.getValue()) {
                String memoryUuid = UuidUtil.toShort(ref.getEmbeddingId());
                if (entry.getKey() == MemoryType.SEMANTIC) {
                    CharacterMemorySemantic row = semanticByUuid.get(memoryUuid);
                    if (row == null) {
                        continue;
                    }
                    dtos.add(RefEmbeddingDto.builder()
                            .embeddingId(ref.getEmbeddingId())
                            .text(row.getContent())
                            .memoryType(MemoryType.SEMANTIC.getDesc())
                            .build());
                } else if (entry.getKey() == MemoryType.EPISODIC) {
                    CharacterMemoryEpisodic row = episodicByUuid.get(memoryUuid);
                    if (row == null) {
                        continue;
                    }
                    dtos.add(RefEmbeddingDto.builder()
                            .embeddingId(ref.getEmbeddingId())
                            .text(row.getContent())
                            .memoryType(MemoryType.EPISODIC.getDesc())
                            .eventType(row.getEventType())
                            .createTime(format(row.getCreateTime()))
                            .importance(row.getImportance())
                            .build());
                } else if (entry.getKey() == MemoryType.INSTRUCTION) {
                    CharacterMemoryInstruction row = instructionByUuid.get(memoryUuid);
                    if (row == null) {
                        continue;
                    }
                    dtos.add(RefEmbeddingDto.builder()
                            .embeddingId(ref.getEmbeddingId())
                            .text(row.getContent())
                            .memoryType(MemoryType.INSTRUCTION.getDesc())
                            .build());
                } else {
                    log.warn("Memory type {} has no store yet, skipping ref {}", ref.getMemoryType(), ref.getEmbeddingId());
                }
            }
        }
        return dtos;
    }

    private static List<String> uuidsOf(List<CharacterMessageRefMemoryEmbedding> refs) {
        return refs.stream().map(ref -> UuidUtil.toShort(ref.getEmbeddingId())).toList();
    }

    private static String format(LocalDateTime time) {
        return time == null ? null : LocalDateTimeUtil.format(time, LocalDateTimeUtil.PATTERN_DEFAULT);
    }
}

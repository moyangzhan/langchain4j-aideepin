package com.moyz.adi.common.memory.longterm;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.CharacterEpisodicMemory;
import com.moyz.adi.common.memory.vo.ExtractedEpisodicEvent;
import com.moyz.adi.common.service.CharacterEpisodicMemoryService;
import com.moyz.adi.common.util.UuidUtil;
import dev.langchain4j.data.document.Metadata;
import dev.langchain4j.data.embedding.Embedding;
import dev.langchain4j.data.segment.TextSegment;
import dev.langchain4j.model.embedding.EmbeddingModel;
import dev.langchain4j.store.embedding.EmbeddingStore;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static com.moyz.adi.common.cosntant.AdiConstant.MetadataKey.*;

/**
 * Append-only service for episodic memory. Each invocation creates new rows in both the
 * vector store (for similarity search) and the relational table (for time-line retrieval).
 * <p>
 * 情景记忆 append-only 写入服务。每次写入同时在向量库（语义检索用）和关系表（时间轴检索用）中创建新行。
 */
@Slf4j
@Service
public class EpisodicMemoryService {

    @Resource
    private EmbeddingStore<TextSegment> characterMemoryEmbeddingStore;
    @Resource
    private EmbeddingModel embeddingModel;
    @Resource
    private CharacterEpisodicMemoryService characterEpisodicMemoryService;

    /**
     * Batch add episodic events — idempotent by sourceMsgId.
     * <p>
     * 批量写入情景事件，按 sourceMsgId 幂等。
     *
     * @param characterId    角色 ID
     * @param userId         用户 ID
     * @param sourceMsgId    源消息 ID（用于幂等判断）
     * @param events         事件列表
     * @param isFreeToken    是否免费模型
     */
    public void batchAdd(Long characterId, Long userId, Long sourceMsgId,
                         List<ExtractedEpisodicEvent> events, boolean isFreeToken) {
        if (CollectionUtils.isEmpty(events)) {
            return;
        }

        // Idempotency: if this source message already produced episodic records, skip.
        // <p>
        // 幂等判断：同一消息已有 episodic 记录则跳过。
        if (sourceMsgId != null && characterEpisodicMemoryService.existsBySourceMsgId(sourceMsgId)) {
            log.info("Episodic events already exist for sourceMsgId={}, skip", sourceMsgId);
            return;
        }

        LocalDateTime now = LocalDateTime.now();
        List<CharacterEpisodicMemory> episodicRows = new ArrayList<>();
        List<String> embeddingIds = new ArrayList<>();
        List<Embedding> embeddings = new ArrayList<>();
        List<TextSegment> segments = new ArrayList<>();

        for (ExtractedEpisodicEvent event : events) {
            if (event == null || StringUtils.isBlank(event.getSummary())) {
                continue;
            }

            String uuid = UuidUtil.createShort();
            Embedding embedding = embeddingModel.embed(event.getSummary()).content();
            String embeddingId = uuid;

            String eventType = StringUtils.isNotBlank(event.getEventType())
                    ? event.getEventType()
                    : AdiConstant.EVENT_TYPE_GENERAL;
            int importance = event.getImportance() != null
                    ? Math.max(1, Math.min(5, event.getImportance()))
                    : AdiConstant.EPISODIC_IMPORTANCE_DEFAULT;

            // Build metadata with null-safe sourceMsgId (Map.of rejects null values).
            // Keep CHARACTER_ID as Long to match the filter type used in IsEqualTo lookups.
            // <p>
            // 构建 metadata，sourceMsgId 需 null 安全（Map.of 拒收 null）。
            // CHARACTER_ID 保持 Long 类型，与 IsEqualTo 过滤条件类型一致。
            Metadata metadata = new Metadata(Map.of(
                    CHARACTER_ID, characterId,
                    MEMORY_TYPE, AdiConstant.MemoryType.EPISODIC,
                    CREATED_AT, now.toString(),
                    EVENT_TYPE, eventType,
                    IMPORTANCE, importance
            ));
            if (sourceMsgId != null) {
                metadata.put(SOURCE_MSG_ID, sourceMsgId);
            }
            TextSegment segment = TextSegment.from(event.getSummary(), metadata);
            embeddingIds.add(embeddingId);
            embeddings.add(embedding);
            segments.add(segment);

            CharacterEpisodicMemory row = new CharacterEpisodicMemory();
            row.setUuid(uuid);
            row.setCharacterId(characterId);
            row.setUserId(userId);
            row.setSummary(event.getSummary());
            row.setSourceMsgId(sourceMsgId);
            row.setEventType(eventType);
            row.setImportance(importance);
            row.setEmbeddingId(embeddingId);
            row.setCreatedAt(now);
            row.setIsActive(true);
            row.setIsDeleted(false);
            episodicRows.add(row);
        }

        if (segments.isEmpty()) {
            log.warn("No valid episodic events to persist");
            return;
        }

        // 关系表先写：作为幂等的事实来源，向量库写失败时可重试不会重复落库。
        // 反过来，若向量库先写而 DB 写失败，下次重试会绕过幂等再次写入向量库，造成重复。
        // <p>
        // Relational table first: it's the source of truth for idempotency. If the vector
        // write fails afterwards we can retry safely (a future retry will hit the idempotency
        // guard and short-circuit). The reverse order would leak duplicate vectors on partial
        // failure because the idempotency guard only sees the relational table.
        characterEpisodicMemoryService.saveBatch(episodicRows);
        try {
            characterMemoryEmbeddingStore.addAll(embeddingIds, embeddings, segments);
        } catch (Exception e) {
            // Roll back the relational rows we just inserted; without rollback, the next attempt
            // will see them and skip via idempotency, leaving these events without any vector.
            // <p>
            // 回滚刚插入的关系行：否则下次重试会被幂等拦截，导致这批事件永远没有对应向量。
            log.error("Vector store write failed, rolling back relational rows. characterId={}, count={}",
                    characterId, episodicRows.size(), e);
            List<Long> ids = episodicRows.stream().map(CharacterEpisodicMemory::getId).filter(java.util.Objects::nonNull).toList();
            if (!ids.isEmpty()) {
                characterEpisodicMemoryService.removeBatchByIds(ids);
            }
            throw e;
        }
        log.info("Episodic memory batch added: characterId={}, count={}", characterId, episodicRows.size());
    }
}
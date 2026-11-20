package com.moyz.adi.common.memory.longterm;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.CharacterMemoryEpisodic;
import com.moyz.adi.common.enums.EventType;
import com.moyz.adi.common.enums.MemoryType;
import com.moyz.adi.common.memory.vo.ExtractedEpisodicEvent;
import com.moyz.adi.common.service.CharacterMemoryEpisodicService;
import com.moyz.adi.common.util.LocalDateTimeUtil;
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
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.moyz.adi.common.cosntant.AdiConstant.MetadataKey.*;

/**
 * Append-only writer for episodic memory, double-writing the relational source of
 * truth first and the dedicated {@code episodicEmbeddingStore} second. The vector
 * row id equals the relational row uuid in canonical 36-char form; content and
 * structured fields live on the relational row, the vector side is a retrieval
 * index joined back by uuid.
 * <p>
 * 情景记忆 append-only 写入服务：先写关系事实源行，再写独立情景向量库（与语义记忆物理
 * 隔离，互不抢占 top-K）。向量行主键 = 关系行 uuid 的 canonical 36 位形；内容与结构化
 * 字段住在关系行，向量侧为按 uuid 联接回表的检索索引。
 */
@Slf4j
@Service
public class EpisodicMemoryService {

    @Resource
    @Qualifier("episodicEmbeddingStore")
    private EmbeddingStore<TextSegment> episodicEmbeddingStore;

    @Resource
    private EmbeddingModel embeddingModel;

    @Resource
    private CharacterMemoryEpisodicService characterMemoryEpisodicService;

    /**
     * Batch add episodic events: relational rows first, then the vector index. No
     * idempotency check is performed: retried calls with the same
     * {@code characterMsgId} produce duplicate rows, which is acceptable for
     * episodic recall since retries are rare and duplicates have negligible
     * impact on LLM analysis quality.
     *
     * @param characterId    character id
     * @param userId         user id (stored on the relational row)
     * @param characterMsgId source character message id, stored for traceability only
     * @param events         events to add
     * @param isFreeToken    whether the model is free-tier (passed through for cost accounting)
     */
    public void batchAdd(Long characterId, Long userId, Long characterMsgId,
                         List<ExtractedEpisodicEvent> events, boolean isFreeToken) {
        if (CollectionUtils.isEmpty(events)) {
            return;
        }

        String createTimeStr = LocalDateTimeUtil.format(LocalDateTime.now(), LocalDateTimeUtil.PATTERN_DEFAULT);
        List<CharacterMemoryEpisodic> rows = new ArrayList<>();
        for (ExtractedEpisodicEvent event : events) {
            if (event == null || StringUtils.isBlank(event.getSummary())) {
                continue;
            }
            CharacterMemoryEpisodic row = new CharacterMemoryEpisodic();
            row.setUuid(UuidUtil.createShort());
            row.setCharacterId(characterId);
            row.setContent(event.getSummary());
            row.setEventType(EventType.fromString(event.getEventType()).getCode());
            row.setImportance(event.getImportance() != null
                    ? Math.max(1, Math.min(5, event.getImportance()))
                    : AdiConstant.EPISODIC_IMPORTANCE_DEFAULT);
            row.setSourceMsgId(characterMsgId != null ? characterMsgId : -1L);
            row.setUserId(userId);
            rows.add(row);
        }

        if (rows.isEmpty()) {
            log.warn("No valid episodic events to persist");
            return;
        }

        characterMemoryEpisodicService.saveBatch(rows);

        List<String> embeddingIds = new ArrayList<>(rows.size());
        List<Embedding> embeddings = new ArrayList<>(rows.size());
        List<TextSegment> segments = new ArrayList<>(rows.size());
        for (CharacterMemoryEpisodic row : rows) {
            Map<String, Object> meta = new HashMap<>();
            meta.put(CHARACTER_ID, characterId);
            meta.put(MEMORY_TYPE, MemoryType.EPISODIC.getDesc());
            meta.put(CREATE_TIME, createTimeStr);
            meta.put(EVENT_TYPE, row.getEventType());
            meta.put(IMPORTANCE, row.getImportance());
            if (row.getSourceMsgId() != null && row.getSourceMsgId() != -1L) {
                meta.put(CHARACTER_MSG_ID, row.getSourceMsgId());
            }
            embeddingIds.add(UuidUtil.toCanonical(row.getUuid()));
            embeddings.add(embeddingModel.embed(row.getContent()).content());
            segments.add(TextSegment.from(row.getContent(), new Metadata(meta)));
        }
        episodicEmbeddingStore.addAll(embeddingIds, embeddings, segments);
        log.info("Episodic memory batch added: characterId={}, count={}", characterId, rows.size());
    }
}

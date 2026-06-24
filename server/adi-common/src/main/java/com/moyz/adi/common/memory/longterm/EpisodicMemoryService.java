package com.moyz.adi.common.memory.longterm;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.enums.EventType;
import com.moyz.adi.common.enums.MemoryType;
import com.moyz.adi.common.memory.vo.ExtractedEpisodicEvent;
import com.moyz.adi.common.util.LocalDateTimeUtil;
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
import java.util.UUID;

import static com.moyz.adi.common.cosntant.AdiConstant.MetadataKey.*;

/**
 * Append-only writer for episodic memory. Each event becomes a row in the dedicated
 * {@code episodicEmbeddingStore} (physically isolated from semantic memory so the
 * two never compete for top-K). All structured fields (event_type, importance,
 * create_time, conv_msg_id) live in the vector store's metadata — no relational table.
 * <p>
 * 情景记忆 append-only 写入服务。事件写入独立的 {@code episodicEmbeddingStore}
 * （与语义记忆物理隔离，互不抢占 top-K 召回）。所有结构化字段（事件类型、重要性、创建时间、
 * 来源消息）都存在向量库 metadata 中，不再需要关系表。
 */
@Slf4j
@Service
public class EpisodicMemoryService {

    @Resource
    @Qualifier("episodicEmbeddingStore")
    private EmbeddingStore<TextSegment> episodicEmbeddingStore;

    @Resource
    private EmbeddingModel embeddingModel;

    /**
     * Batch add episodic events to the dedicated episodic vector store. No
     * idempotency check is performed: retried calls with the same
     * {@code characterMsgId} produce duplicate rows, which is acceptable for
     * episodic recall since retries are rare and duplicates have negligible
     * impact on LLM analysis quality.
     *
     * @param characterId    character id
     * @param userId         user id (reserved for future per-user filtering)
     * @param characterMsgId source character message id, stored in metadata for traceability only
     * @param events         events to add
     * @param isFreeToken    whether the model is free-tier (passed through for cost accounting)
     */
    public void batchAdd(Long characterId, Long userId, Long characterMsgId,
                         List<ExtractedEpisodicEvent> events, boolean isFreeToken) {
        if (CollectionUtils.isEmpty(events)) {
            return;
        }
        // userId / isFreeToken are reserved for future per-user filtering and cost
        // accounting; they're carried through the signature so callers don't have to
        // change when those features arrive.

        // Format once per batch with project-wide pattern (yyyy-MM-dd HH:mm:ss).
        // Sub-second precision adds no recall value for "past events"; the canonical
        // format is also the lingua franca used elsewhere in the codebase / UI.
        // 每批格式化一次，使用项目统一的 yyyy-MM-dd HH:mm:ss——亚秒精度对"过往事件"
        // 召回无价值，此格式与代码库/前端其他时间字段一致。
        String createTimeStr = LocalDateTimeUtil.format(LocalDateTime.now(), LocalDateTimeUtil.PATTERN_DEFAULT);
        List<String> embeddingIds = new ArrayList<>();
        List<Embedding> embeddings = new ArrayList<>();
        List<TextSegment> segments = new ArrayList<>();

        for (ExtractedEpisodicEvent event : events) {
            if (event == null || StringUtils.isBlank(event.getSummary())) {
                continue;
            }

            // PgVector requires a canonical UUID string (parsed via UUID.fromString);
            // the project-wide UuidUtil.createShort() strips dashes and would fail here.
            String embeddingId = UUID.randomUUID().toString();
            Embedding embedding = embeddingModel.embed(event.getSummary()).content();

            String eventType = EventType.fromString(event.getEventType()).getCode();
            int importance = event.getImportance() != null
                    ? Math.max(1, Math.min(5, event.getImportance()))
                    : AdiConstant.EPISODIC_IMPORTANCE_DEFAULT;

            // Build metadata with a mutable HashMap so characterMsgId can be conditionally
            // added (Map.of rejects null values). CHARACTER_ID stays as Long to match
            // the type used by IsEqualTo filters.
            Map<String, Object> meta = new HashMap<>();
            meta.put(CHARACTER_ID, characterId);
            meta.put(MEMORY_TYPE, MemoryType.EPISODIC.getDesc());
            meta.put(CREATE_TIME, createTimeStr);
            meta.put(EVENT_TYPE, eventType);
            meta.put(IMPORTANCE, importance);
            if (characterMsgId != null) {
                meta.put(CHARACTER_MSG_ID, characterMsgId);
            }
            Metadata metadata = new Metadata(meta);
            TextSegment segment = TextSegment.from(event.getSummary(), metadata);

            embeddingIds.add(embeddingId);
            embeddings.add(embedding);
            segments.add(segment);
        }

        if (segments.isEmpty()) {
            log.warn("No valid episodic events to persist");
            return;
        }

        episodicEmbeddingStore.addAll(embeddingIds, embeddings, segments);
        log.info("Episodic memory batch added: characterId={}, count={}", characterId, segments.size());
    }
}


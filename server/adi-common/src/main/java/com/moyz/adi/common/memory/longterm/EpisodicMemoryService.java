package com.moyz.adi.common.memory.longterm;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.enums.EventType;
import com.moyz.adi.common.memory.vo.ExtractedEpisodicEvent;
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
 * Append-only writer for episodic memory. Each event becomes a row in the dedicated
 * {@code episodicMemoryEmbeddingStore} (physically isolated from semantic memory so the
 * two never compete for top-K). All structured fields (event_type, importance,
 * created_at, source_msg_id) live in the vector store's metadata — no relational table.
 * <p>
 * 情景记忆 append-only 写入服务。事件写入独立的 {@code episodicMemoryEmbeddingStore}
 * （与语义记忆物理隔离，互不抢占 top-K 召回）。所有结构化字段（事件类型、重要性、创建时间、
 * 来源消息）都存在向量库 metadata 中，不再需要关系表。
 */
@Slf4j
@Service
public class EpisodicMemoryService {

    @Resource
    @Qualifier("episodicMemoryEmbeddingStore")
    private EmbeddingStore<TextSegment> episodicMemoryEmbeddingStore;

    @Resource
    private EmbeddingModel embeddingModel;

    /**
     * Batch add episodic events to the dedicated episodic vector store.
     * <p>
     * Idempotency was previously enforced via a relational table; it has been dropped
     * in favour of architectural simplicity. The trade-off is that retried calls with
     * the same {@code sourceMsgId} will produce duplicate episodic rows — acceptable
     * because (a) retries are rare and (b) duplicate episodic events have negligible
     * impact on LLM analysis quality.
     * <p>
     * 批量写入情景事件到独立向量库。原先按 sourceMsgId 做的幂等检查已被移除以换取架构简化：
     * 同一消息的重试会产生重复的 episodic 记录，但重试罕见且重复对 LLM 分析质量影响可忽略。
     *
     * @param characterId 角色 ID
     * @param userId      用户 ID
     * @param sourceMsgId 源消息 ID（仅写入 metadata 用于回溯，不再做幂等）
     * @param events      事件列表
     * @param isFreeToken 是否免费模型（保留入参，调度器侧用于成本结算时透传）
     */
    public void batchAdd(Long characterId, Long userId, Long sourceMsgId,
                         List<ExtractedEpisodicEvent> events, boolean isFreeToken) {
        if (CollectionUtils.isEmpty(events)) {
            return;
        }
        // userId / isFreeToken are reserved for future per-user filtering and cost
        // accounting; they're carried through the signature so callers don't have to
        // change when those features arrive.
        // <p>
        // userId / isFreeToken 保留入参以便未来支持用户级过滤和成本结算，避免后续改签名。

        LocalDateTime now = LocalDateTime.now();
        List<String> embeddingIds = new ArrayList<>();
        List<Embedding> embeddings = new ArrayList<>();
        List<TextSegment> segments = new ArrayList<>();

        for (ExtractedEpisodicEvent event : events) {
            if (event == null || StringUtils.isBlank(event.getSummary())) {
                continue;
            }

            String embeddingId = UuidUtil.createShort();
            Embedding embedding = embeddingModel.embed(event.getSummary()).content();

            String eventType = EventType.fromString(event.getEventType()).getCode();
            int importance = event.getImportance() != null
                    ? Math.max(1, Math.min(5, event.getImportance()))
                    : AdiConstant.EPISODIC_IMPORTANCE_DEFAULT;

            // Build metadata. Use a mutable HashMap so sourceMsgId can be conditionally
            // added (Map.of rejects null values). CHARACTER_ID stays as Long to match
            // the type used by IsEqualTo filters.
            // <p>
            // 用可变 Map 构建 metadata：sourceMsgId 可能为 null，Map.of 不接受 null。
            // CHARACTER_ID 保持 Long 以匹配 IsEqualTo 过滤条件类型。
            Map<String, Object> meta = new HashMap<>();
            meta.put(CHARACTER_ID, characterId);
            meta.put(MEMORY_TYPE, AdiConstant.MemoryType.EPISODIC);
            meta.put(CREATED_AT, now.toString());
            meta.put(EVENT_TYPE, eventType);
            meta.put(IMPORTANCE, importance);
            if (sourceMsgId != null) {
                meta.put(SOURCE_MSG_ID, sourceMsgId);
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

        episodicMemoryEmbeddingStore.addAll(embeddingIds, embeddings, segments);
        log.info("Episodic memory batch added: characterId={}, count={}", characterId, segments.size());
    }
}

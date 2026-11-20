package com.moyz.adi.common.memory.longterm;

import com.moyz.adi.common.config.AdiProperties;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.CharacterMemorySemantic;
import com.moyz.adi.common.enums.MemoryType;
import com.moyz.adi.common.memory.vo.BatchActionMemories;
import com.moyz.adi.common.service.CharacterMemorySemanticService;
import com.moyz.adi.common.util.UuidUtil;
import dev.langchain4j.data.document.Metadata;
import dev.langchain4j.data.embedding.Embedding;
import dev.langchain4j.data.segment.TextSegment;
import dev.langchain4j.model.embedding.EmbeddingModel;
import dev.langchain4j.store.embedding.EmbeddingMatch;
import dev.langchain4j.store.embedding.EmbeddingSearchRequest;
import dev.langchain4j.store.embedding.EmbeddingSearchResult;
import dev.langchain4j.store.embedding.EmbeddingStore;
import dev.langchain4j.store.embedding.filter.comparison.IsEqualTo;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static com.moyz.adi.common.cosntant.AdiConstant.MetadataKey.CHARACTER_ID;

/**
 * Semantic memory service: deduplicating merge candidates retrieval and
 * ADD / UPDATE / DELETE execution, double-writing the relational source of truth
 * first and the semantic vector store second (the vector row id equals the
 * relational row uuid in canonical form). ADD embeds the whole fact as one
 * segment; UPDATE re-embeds the new text under the same uuid; DELETE removes the
 * vector row first, then soft-deletes the relational row. Does not call LLMs and
 * does not account for cost — the caller (dispatcher) drives those.
 * <p>
 * 语义记忆服务：合并候选检索与 ADD / UPDATE / DELETE 执行，先写关系事实源行、再写语义
 * 向量库（向量行主键 = 关系行 uuid 的 canonical 形）。ADD 整条 fact 一次直嵌；UPDATE
 * 同 uuid 重嵌新文本；DELETE 先移向量行、后软删关系行。不调 LLM、不计 token 成本，
 * 由调度器统筹这些事项。
 */
@Slf4j
@Service
public class SemanticMemoryService {

    @Resource
    @Qualifier("semanticEmbeddingStore")
    private EmbeddingStore<TextSegment> semanticEmbeddingStore;

    @Resource
    private EmbeddingModel embeddingModel;

    @Resource
    private AdiProperties adiProperties;

    @Resource
    private CharacterMemorySemanticService characterMemorySemanticService;

    /**
     * For each fact, find its top-K nearest neighbours in the semantic store. Each
     * fact keeps its own tmpId space so the LLM never sees ambiguous IDs across facts.
     * <p>
     * 对每条 fact 检索 top-K 候选。每条 fact 独立的 tmpId 空间，避免 LLM 跨 fact 时 id 歧义。
     */
    public MergeCandidates searchMergeCandidates(List<String> facts, Long characterId) {
        Map<String, List<Map<String, String>>> factToOldMemoriesForPrompt = new LinkedHashMap<>();
        Map<String, Map<String, String>> factToTmpIdToEmbeddingId = new HashMap<>();
        double minScore = adiProperties.getMemory() != null
                ? adiProperties.getMemory().getMinScore()
                : AdiConstant.LONG_TERM_MEMORY_MIN_SCORE_DEFAULT;

        for (String fact : facts) {
            if (StringUtils.isBlank(fact)) {
                continue;
            }
            Embedding embedding = embeddingModel.embed(fact).content();
            EmbeddingSearchRequest searchRequest = EmbeddingSearchRequest.builder()
                    .queryEmbedding(embedding)
                    .maxResults(5)
                    .minScore(minScore)
                    .filter(new IsEqualTo(CHARACTER_ID, characterId))
                    .build();
            EmbeddingSearchResult<TextSegment> searchResult = semanticEmbeddingStore.search(searchRequest);

            Map<String, EmbeddingMatch<TextSegment>> embeddingIdToMatch = new LinkedHashMap<>();
            searchResult.matches().forEach(item -> embeddingIdToMatch.put(item.embeddingId(), item));

            List<Map<String, String>> oldMemoryForPrompt = new ArrayList<>();
            Map<String, String> tmpIdToEmbeddingId = new HashMap<>();
            int i = 0;
            for (Map.Entry<String, EmbeddingMatch<TextSegment>> entry : embeddingIdToMatch.entrySet()) {
                String tmpId = String.valueOf(i);
                oldMemoryForPrompt.add(Map.of("id", tmpId, "text", entry.getValue().embedded().text()));
                tmpIdToEmbeddingId.put(tmpId, entry.getKey());
                i++;
            }

            factToOldMemoriesForPrompt.put(fact, oldMemoryForPrompt);
            factToTmpIdToEmbeddingId.put(fact, tmpIdToEmbeddingId);
        }
        return new MergeCandidates(factToOldMemoriesForPrompt, factToTmpIdToEmbeddingId);
    }

    /**
     * Apply ADD / UPDATE / DELETE actions to semantic memory. Single-action failures
     * are caught here so other actions still proceed.
     * <p>
     * 应用 ADD / UPDATE / DELETE 操作。单条失败不影响其他操作。
     */
    public void applyActions(BatchActionMemories batchActions, Long characterId, Long userId, MergeCandidates candidates) {
        if (batchActions == null || batchActions.getActions() == null) {
            return;
        }
        for (BatchActionMemories.BatchActionMemory action : batchActions.getActions()) {
            try {
                applyAction(action, characterId, userId, candidates);
            } catch (Exception ex) {
                log.warn("Apply memory action failed, action={}, error={}", action, ex.getMessage());
            }
        }
    }

    private void applyAction(BatchActionMemories.BatchActionMemory action,
                             Long characterId,
                             Long userId,
                             MergeCandidates candidates) {
        if (action == null || StringUtils.isBlank(action.getEvent())) {
            return;
        }
        String event = action.getEvent().trim().toUpperCase();
        if (AdiConstant.MemoryEvent.NONE.equalsIgnoreCase(event)) {
            log.info("No changes required for memory id: {}", action.getId());
            return;
        }

        if (AdiConstant.MemoryEvent.ADD.equalsIgnoreCase(event)) {
            if (StringUtils.isBlank(action.getText())) {
                log.warn("ADD action missing text, skip");
                return;
            }
            addMemory(characterId, userId, action.getText());
            return;
        }

        Map<String, String> tmpIdToEmbeddingId = candidates.factToTmpIdToEmbeddingId.get(action.getFact());
        if (tmpIdToEmbeddingId == null) {
            log.warn("LLM hallucinated fact field, no matching context, fact={}, skip", action.getFact());
            return;
        }
        String tmpId = action.getId();
        if (StringUtils.isBlank(tmpId)) {
            log.warn("UPDATE/DELETE action missing id, action={}, skip", action);
            return;
        }
        String embeddingId = tmpIdToEmbeddingId.get(tmpId);
        if (embeddingId == null) {
            log.warn("LLM hallucinated id, tmpId={} not in mapping, fact={}, skip", tmpId, action.getFact());
            return;
        }

        if (AdiConstant.MemoryEvent.DELETE.equalsIgnoreCase(event)) {
            deleteMemory(embeddingId);
            return;
        }

        if (AdiConstant.MemoryEvent.UPDATE.equalsIgnoreCase(event)) {
            if (StringUtils.isBlank(action.getText())) {
                log.warn("UPDATE action missing text, action={}, skip", action);
                return;
            }
            updateMemory(embeddingId, characterId, userId, action.getText());
            return;
        }

        log.warn("Unknown memory event: {}, action={}", event, action);
    }

    private void addMemory(Long characterId, Long userId, String text) {
        CharacterMemorySemantic row = new CharacterMemorySemantic();
        row.setUuid(UuidUtil.createShort());
        row.setCharacterId(characterId);
        row.setContent(text);
        row.setUserId(userId);
        characterMemorySemanticService.save(row);
        addToVector(row.getUuid(), characterId, text, embeddingModel.embed(text).content());
    }

    private void updateMemory(String embeddingId, Long characterId, Long userId, String text) {
        String uuid = UuidUtil.toShort(embeddingId);
        Embedding newEmbedding = embeddingModel.embed(text).content();
        characterMemorySemanticService.upsertContent(uuid, characterId, text, userId);
        semanticEmbeddingStore.remove(embeddingId);
        addToVector(uuid, characterId, text, newEmbedding);
    }

    private void deleteMemory(String embeddingId) {
        semanticEmbeddingStore.remove(embeddingId);
        characterMemorySemanticService.softDeleteByUuid(UuidUtil.toShort(embeddingId));
    }

    private void addToVector(String uuid, Long characterId, String text, Embedding embedding) {
        Metadata metadata = new Metadata(Map.of(
                CHARACTER_ID, characterId,
                AdiConstant.MetadataKey.MEMORY_TYPE, MemoryType.SEMANTIC.getDesc()));
        semanticEmbeddingStore.addAll(List.of(UuidUtil.toCanonical(uuid)),
                List.of(embedding), List.of(TextSegment.from(text, metadata)));
    }

    /**
     * Merge candidates collected per-fact. Exposes the per-fact prompt input
     * for the dispatcher to feed the LLM, and keeps the tmpId↔embeddingId mapping
     * needed for {@link #applyActions}.
     * <p>
     * 按 fact 聚合的合并候选。两个 map 分别给调度器构造 prompt、给 applyActions 反查使用。
     */
    public static final class MergeCandidates {
        public final Map<String, List<Map<String, String>>> factToOldMemoriesForPrompt;
        public final Map<String, Map<String, String>> factToTmpIdToEmbeddingId;

        public MergeCandidates(Map<String, List<Map<String, String>>> factToOldMemoriesForPrompt,
                               Map<String, Map<String, String>> factToTmpIdToEmbeddingId) {
            this.factToOldMemoriesForPrompt = factToOldMemoriesForPrompt;
            this.factToTmpIdToEmbeddingId = factToTmpIdToEmbeddingId;
        }

        public boolean isEmpty() {
            return factToOldMemoriesForPrompt.isEmpty();
        }
    }
}

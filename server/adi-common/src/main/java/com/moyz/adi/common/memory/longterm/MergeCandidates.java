package com.moyz.adi.common.memory.longterm;

import java.util.List;
import java.util.Map;

/**
 * Merge candidates collected per-fact for one memory channel. Exposes the per-fact
 * prompt input for the dispatcher to feed the LLM, and keeps the tmpId↔embeddingId
 * mapping needed when applying the actions.
 * <p>
 * 单一记忆通道按 fact 聚合的合并候选。两个 map 分别给调度器构造 prompt、给动作执行反查使用。
 */
public final class MergeCandidates {
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

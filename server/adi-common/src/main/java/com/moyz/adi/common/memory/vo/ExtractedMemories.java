package com.moyz.adi.common.memory.vo;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.util.List;

/**
 * Dual-task extraction result from the LLM.
 * Semantic facts and episodic events are extracted independently in a single LLM call.
 * <p>
 * 双任务提取结果。semantic 和 episodic 在一次 LLM 调用中独立抽取。
 * Backward-compatible: if parsing fails, caller falls back to old {@link ExtractedFact} format.
 */
@Data
public class ExtractedMemories {

    /**
     * 语义事实列表（稳定、可合并的知识）。 | Semantic facts (stable, mergeable knowledge).
     */
    @JsonProperty("semantic_facts")
    private List<String> semanticFacts;

    /**
     * 情景事件列表（绑定时间线、不可合并的事件）。 | Episodic events (timeline-bound, non-mergeable).
     */
    @JsonProperty("episodic_events")
    private List<ExtractedEpisodicEvent> episodicEvents;
}

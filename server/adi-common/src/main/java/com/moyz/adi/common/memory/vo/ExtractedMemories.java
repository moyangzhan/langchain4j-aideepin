package com.moyz.adi.common.memory.vo;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.util.List;

/**
 * Triple-task extraction result from the LLM.
 * Semantic facts, episodic events and instruction rules are extracted independently
 * in a single LLM call.
 * <p>
 * 三任务提取结果。semantic、episodic、instruction 在一次 LLM 调用中独立抽取。
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

    /**
     * 指令规则列表（用户教给助手的、面向助手行为的可合并规则）。 | Instruction rules
     * (mergeable behaviour rules for the assistant, taught by the user).
     */
    @JsonProperty("instructions")
    private List<String> instructions;
}

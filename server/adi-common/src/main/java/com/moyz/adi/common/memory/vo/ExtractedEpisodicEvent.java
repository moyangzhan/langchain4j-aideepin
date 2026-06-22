package com.moyz.adi.common.memory.vo;

import lombok.Data;

/**
 * Single episodic event extracted from a conversation by the LLM.
 * <p>
 * 单条情景事件，由 LLM 从对话中提取。
 */
@Data
public class ExtractedEpisodicEvent {

    /**
     * 事件摘要文本。 | Event summary text.
     */
    private String summary;

    /**
     * 事件类型。LLM 判定，如 travel / health / work / general。 | Event type.
     */
    private String eventType;

    /**
     * 重要性 1-5。 | Importance 1-5.
     */
    private Integer importance;
}
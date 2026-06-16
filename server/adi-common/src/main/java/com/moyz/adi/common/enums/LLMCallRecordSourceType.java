package com.moyz.adi.common.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * LLM 调用记录的来源类型 | Source type of LLM call record
 */
@Getter
@AllArgsConstructor
public enum LLMCallRecordSourceType implements BaseEnum {

    UNKNOWN(0, "unknown"),
    CHARACTER_CHAT(1, "character_chat"),
    KNOWLEDGE_BASE_QA(2, "knowledge_base_qa"),
    KNOWLEDGE_BASE_INGEST(3, "knowledge_base_ingest"),
    WORKFLOW_NODE(4, "workflow_node"),
    AGENT(5, "agent"),
    LONG_TERM_MEMORY(6, "long_term_memory");

    private final Integer value;
    private final String desc;
}

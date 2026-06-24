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
    // Each value maps 1:1 to a single LLM request within the long-term memory flow,
    // mirroring how KNOWLEDGE_BASE_QA / _INGEST split the knowledge base flow.
    LONG_TERM_MEMORY_EXTRACTION(6, "long_term_memory_extraction"),
    LONG_TERM_MEMORY_ANALYSIS(7, "long_term_memory_analysis");

    private final Integer value;
    private final String desc;
}

package com.moyz.adi.common.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Result of executing a Character in agent mode.
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class AgentResult {

    /** The agent's text response. */
    private String answer;

    /** Thinking content from reasoning models, may be null. */
    private String thinking;

    /** Input token count. */
    private Integer inputTokens;

    /** Output token count. */
    private Integer outputTokens;

    /** Knowledge retrieval result count. */
    private Integer retrievalCount;
}

package com.moyz.adi.common.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Request DTO for executing a Character in agent mode.
 *
 * <p>Specifies which Character to execute and which capabilities to enable
 * (RAG, MCP tools, web search). Decoupled from workflow-specific configuration.</p>
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class AgentRequest {

    /** Character UUID to execute. */
    private String characterUuid;

    /** Model platform (optional, uses system default if null). */
    private String modelPlatform;

    /** Model name (optional, uses system default if null). */
    private String modelName;

    /** Input text (rendered prompt). */
    private String inputText;

    /** Enable RAG retrieval from Character's knowledge bases. */
    private boolean enableRag;

    /** Enable MCP tools configured on the Character. */
    private boolean enableMcp;

    /** Enable web search. */
    private boolean enableWebSearch;
}

package com.moyz.adi.common.workflow.node.agent;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

/**
 * Agent 节点配置
 * <p>
 * Agent node configuration — defines which Character to invoke and which capabilities to enable.
 * </p>
 */
@Data
public class AgentNodeConfig {

    /**
     * 角色 UUID（必填）
     * <p>
     * Character UUID (required).
     * </p>
     */
    @NotBlank
    @JsonProperty("character_uuid")
    private String characterUuid;

    /**
     * 模型平台（可选，为空则使用系统默认模型）
     * <p>
     * Model platform (optional, uses system default if null).
     * </p>
     */
    @JsonProperty("model_platform")
    private String modelPlatform;

    /**
     * 模型名称（可选，为空则使用系统默认模型）
     * <p>
     * Model name (optional, uses system default if null).
     * </p>
     */
    @JsonProperty("model_name")
    private String modelName;

    /**
     * 提示词模板（可选，为空则直接使用上游输入）
     * <p>
     * Prompt template (optional, uses upstream input directly if blank).
     * </p>
     */
    private String prompt;

    /**
     * 是否启用 RAG 检索（默认 true）
     * <p>
     * Enable RAG retrieval (default true).
     * </p>
     */
    @JsonProperty("enable_rag")
    private Boolean enableRag = true;

    /**
     * 是否启用 MCP 工具（默认 true）
     * <p>
     * Enable MCP tools (default true).
     * </p>
     */
    @JsonProperty("enable_mcp")
    private Boolean enableMcp = true;

    /**
     * 是否启用联网搜索（默认 false）
     * <p>
     * Enable web search (default false).
     * </p>
     */
    @JsonProperty("enable_web_search")
    private Boolean enableWebSearch = false;
}

package com.moyz.adi.common.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Agent 调用请求
 * <p>
 * Generic request DTO for invoking an Agent (Character).
 * Decoupled from workflow-specific configuration.
 * </p>
 * <p>
 * Agent 调用请求 DTO，与工作流节点配置解耦。
 * </p>
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class AgentRequest {

    /**
     * 角色 UUID
     * <p>
     * Character UUID.
     * </p>
     */
    private String characterUuid;

    /**
     * 模型平台（可选）
     * <p>
     * Model platform (optional, uses system default if null).
     * </p>
     */
    private String modelPlatform;

    /**
     * 模型名称（可选）
     * <p>
     * Model name (optional, uses system default if null).
     * </p>
     */
    private String modelName;

    /**
     * 输入文本（已渲染的 prompt）
     * <p>
     * Input text (rendered prompt).
     * </p>
     */
    private String inputText;

    /**
     * 是否启用 RAG 检索
     * <p>
     * Enable RAG retrieval.
     * </p>
     */
    private boolean enableRag;

    /**
     * 是否启用 MCP 工具
     * <p>
     * Enable MCP tools.
     * </p>
     */
    private boolean enableMcp;

    /**
     * 是否启用联网搜索
     * <p>
     * Enable web search.
     * </p>
     */
    private boolean enableWebSearch;
}

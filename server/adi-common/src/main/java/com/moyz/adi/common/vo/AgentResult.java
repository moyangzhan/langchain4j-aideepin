package com.moyz.adi.common.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Agent 调用结果
 * <p>
 * Result of an agent (Character) invocation.
 * </p>
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class AgentResult {

    /**
     * Agent 的文本回复
     * <p>
     * The agent's text response.
     * </p>
     */
    private String answer;

    /**
     * 推理模型的思考内容，可为空
     * <p>
     * Thinking content from reasoning models, may be null.
     * </p>
     */
    private String thinking;

    /**
     * 输入 token 数量
     * <p>
     * Input token count.
     * </p>
     */
    private Integer inputTokens;

    /**
     * 输出 token 数量
     * <p>
     * Output token count.
     * </p>
     */
    private Integer outputTokens;

    /**
     * 知识检索结果数量
     * <p>
     * Knowledge retrieval result count.
     * </p>
     */
    private Integer retrievalCount;
}

package com.moyz.adi.common.workflow;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.io.Serializable;

/**
 * 节点执行可观测指标 | Node execution observability metrics
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class NodeExecutionMetrics implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    // 通用 | Common
    private long durationMs;

    // LLM 相关 | LLM-related (LLMAnswer, Classifier, KeywordExtractor, FaqExtractor)
    private Integer inputTokens;
    private Integer outputTokens;
    private String modelName;
    private String modelPlatform;

    // HTTP 请求 | HTTP Request
    private Integer httpStatusCode;
    private String httpMethod;

    // 搜索（Google）| Search
    private Integer searchResultCount;

    // 知识库检索 | Knowledge Retrieval
    private Integer retrievalCount;
}

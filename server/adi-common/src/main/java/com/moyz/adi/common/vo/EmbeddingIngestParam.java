package com.moyz.adi.common.vo;

import dev.langchain4j.model.chat.ChatModel;
import lombok.Builder;
import lombok.Getter;

/**
 * Parameters for embedding ingest configuration (excluding the document itself).
 * <p>
 * Embedding 入库的配置参数（不含文档本身）。
 */
@Getter
@Builder
public class EmbeddingIngestParam {

    /**
     * Overlap tokens between adjacent segments.
     * <p>
     * 相邻分段之间的重叠 token 数。
     */
    private final int overlap;

    /**
     * Split strategy: recursive, paragraph, line, sentence, custom.
     * <p>
     * 分块策略：recursive / paragraph / line / sentence / custom。
     */
    private final String strategy;

    /**
     * Maximum segment size in tokens.
     * <p>
     * 单个分段的最大 token 数。
     */
    private final int maxSegmentSize;

    /**
     * Custom separator used when strategy is "custom".
     * <p>
     * 当分块策略为 "custom" 时使用的自定义分隔符。
     */
    private final String customSeparator;

    /**
     * Token estimator name (nullable).
     * <p>
     * Token 估算器名称（可为空）。
     */
    private final String tokenEstimator;

    /**
     * Chat model used for advanced processing during ingestion (nullable).
     * <p>
     * 用于入库过程中高级处理的对话模型（可为空）。
     */
    private final ChatModel chatModel;
}

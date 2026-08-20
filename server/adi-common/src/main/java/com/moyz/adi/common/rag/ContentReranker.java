package com.moyz.adi.common.rag;

import dev.langchain4j.rag.content.Content;

import java.util.List;

/**
 * 重排序扩展点（本期仅预留，不接模型）。
 * <p>
 * 默认实现 {@link NoOpContentReranker} 直通返回。后续接入 rerank 模型时
 * 提供新实现并注入 {@link SegmentExpandProcessor} 即可，无需改动检索链路；
 * 届时配套配置项 adi.rag.rerank.enabled 控制开关。
 */
public interface ContentReranker {

    /**
     * 对展开后的内容重排序（输入已按召回分数降序）
     *
     * @param queryText 用户问题
     * @param contents  展开后的内容列表
     * @return 重排后的内容列表
     */
    List<Content> rerank(String queryText, List<Content> contents);
}

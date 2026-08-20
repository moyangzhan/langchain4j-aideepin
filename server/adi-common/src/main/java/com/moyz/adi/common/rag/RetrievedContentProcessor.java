package com.moyz.adi.common.rag;

import dev.langchain4j.data.segment.TextSegment;
import dev.langchain4j.rag.content.Content;
import dev.langchain4j.rag.query.Query;
import dev.langchain4j.store.embedding.EmbeddingMatch;

import java.util.List;
import java.util.Map;

/**
 * 向量召回结果的后置处理器：在 ANN 检索之后、返回给调用方之前应用。
 * <p>
 * 知识库场景用 {@link SegmentExpandProcessor} 做"按模式展开"（问题→答案、子块→父段上卷去重）；
 * 角色记忆检索器不设置处理器，行为不变。
 * <p>
 * 契约：处理器负责把"保留下来的命中"写入 embeddingToScore（供溯源与命中统计使用），
 * 未写入的命中视为被丢弃，不会产生引用记录。
 */
public interface RetrievedContentProcessor {

    /**
     * @param query            当前查询
     * @param matches          召回的原始命中（按分数降序）
     * @param embeddingToScore 命中分数记录表（初始为空，由处理器填充保留下来的命中）
     * @return 展开后的内容列表
     */
    List<Content> process(Query query, List<EmbeddingMatch<TextSegment>> matches, Map<String, Double> embeddingToScore);
}

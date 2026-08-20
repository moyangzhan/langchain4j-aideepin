package com.moyz.adi.common.service.embedding;

import java.util.List;

/**
 * 知识库向量表的清理职责（按文档/按条目删除向量行）。
 * <p>
 * 分段内容、命中统计、溯源展示等段级职责已迁移到关系层 DocumentSegmentService
 * （adi_document_segment / _question / _child_chunk），向量表退化为纯检索索引，
 * 文本置空，不再维护 hit_count/word_count。
 */
public interface IKnowledgeEmbeddingService {

    /**
     * 删除某文档的全部向量行（重索引/删除文档时调用，按 metadata kb_item_uuid 过滤）
     */
    boolean deleteByItemUuid(String kbItemUuid);

    /**
     * 按向量条目id删除单条/多条向量行（单段重新向量化时清理旧向量）
     */
    boolean deleteByIds(List<String> embeddingIds);
}

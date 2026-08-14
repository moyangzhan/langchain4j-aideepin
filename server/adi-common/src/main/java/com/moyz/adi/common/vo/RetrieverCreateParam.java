package com.moyz.adi.common.vo;

import dev.langchain4j.model.chat.ChatModel;
import dev.langchain4j.store.embedding.filter.Filter;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Set;

/**
 * Content Retriever创建参数
 */
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RetrieverCreateParam {
    /**
     * 用来生成图谱，生成向量时不使用
     */
    private ChatModel chatModel;
    /**
     * 过滤条件
     */
    private Filter filter;
    /**
     * 最大返回数量
     */
    private int maxResults;
    /**
     * 最小命中分数
     */
    private double minScore;
    /**
     * 如果数据库中搜索不到数据，是否强行中断该搜索，不继续往下执行（即不继续请求LLM进行回答）
     */
    private boolean breakIfSearchMissed;
    /**
     * 停用文档的 UUID 集合。向量路径将其组合为 IsNotIn filter 在数据库层面排除；
     * 图谱路径在 Java 层后置过滤（因 kb_item_uuid 可能为逗号分隔多值）。
     */
    private Set<String> excludedItemUuids;
}

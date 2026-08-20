package com.moyz.adi.common.rag;

import dev.langchain4j.rag.content.Content;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * 默认直通实现：不重排序，按展开后的顺序返回。
 * 后续接入 rerank 模型时以 @Primary 新实现替换本 Bean。
 */
@Component
public class NoOpContentReranker implements ContentReranker {

    @Override
    public List<Content> rerank(String queryText, List<Content> contents) {
        return contents;
    }
}

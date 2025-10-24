package com.moyz.adi.common.vo;

import dev.langchain4j.rag.content.Content;
import dev.langchain4j.rag.content.retriever.ContentRetriever;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Builder
@NoArgsConstructor
@AllArgsConstructor
@Data
public class RetrieverWrapper {
    private ContentRetriever retriever;
    //Retrieve content from: knowledge_base conversation_memory web
    private String contentFrom;
    private List<Content> response;
}

package com.moyz.adi.common.interfaces;

import dev.langchain4j.data.document.Document;
import dev.langchain4j.model.chat.ChatModel;
import dev.langchain4j.rag.content.retriever.ContentRetriever;
import dev.langchain4j.store.embedding.filter.Filter;

import java.util.Map;

public interface IRAGService {
    void ingest(Document document, int overlap, String tokenizer, ChatModel ChatModel);

    ContentRetriever createRetriever(Filter filter, int maxResults, double minScore, boolean breakIfSearchMissed);
}

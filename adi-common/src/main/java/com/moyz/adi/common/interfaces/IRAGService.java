package com.moyz.adi.common.interfaces;

import dev.langchain4j.data.document.Document;
import dev.langchain4j.model.chat.ChatLanguageModel;
import dev.langchain4j.rag.content.retriever.ContentRetriever;

import java.util.Map;

public interface IRAGService {
    void ingest(Document document, int overlap, ChatLanguageModel chatLanguageModel);

    ContentRetriever createRetriever(Map<String, String> metadataCond, int maxResults, double minScore, boolean breakIfSearchMissed);
}

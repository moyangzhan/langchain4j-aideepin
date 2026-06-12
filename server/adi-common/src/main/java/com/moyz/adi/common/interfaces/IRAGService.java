package com.moyz.adi.common.interfaces;

import com.moyz.adi.common.vo.EmbeddingIngestParam;
import com.moyz.adi.common.vo.RetrieverCreateParam;
import dev.langchain4j.data.document.Document;
import dev.langchain4j.rag.content.retriever.ContentRetriever;

public interface IRAGService {
    void ingest(Document document, EmbeddingIngestParam params);

    ContentRetriever createRetriever(RetrieverCreateParam param);
}

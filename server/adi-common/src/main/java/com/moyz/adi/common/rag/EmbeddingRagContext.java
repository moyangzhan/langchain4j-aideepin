package com.moyz.adi.common.rag;

import java.util.HashMap;
import java.util.Map;

public class EmbeddingRagContext {

    protected static final Map<String, EmbeddingRag> NAME_TO_RAG = new HashMap<>();

    private EmbeddingRagContext() {
    }

    public static EmbeddingRag get(String name) {
        return NAME_TO_RAG.get(name);
    }

    public static void add(EmbeddingRag rag) {
        NAME_TO_RAG.put(rag.getName(), rag);
    }
}

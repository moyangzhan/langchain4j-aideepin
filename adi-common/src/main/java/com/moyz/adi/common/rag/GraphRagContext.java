package com.moyz.adi.common.rag;

import java.util.HashMap;
import java.util.Map;

public class GraphRagContext {

    protected static final Map<String, GraphRag> NAME_TO_RAG = new HashMap<>();

    private GraphRagContext() {

    }

    public static GraphRag get(String name) {
        return NAME_TO_RAG.get(name);
    }

    public static void add(GraphRag rag) {
        NAME_TO_RAG.put(rag.getName(), rag);
    }
}

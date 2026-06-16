package com.moyz.adi.common.vo;

import com.moyz.adi.common.entity.KnowledgeBaseQa;
import com.moyz.adi.common.entity.User;
import dev.langchain4j.rag.content.retriever.ContentRetriever;
import jakarta.annotation.Nullable;
import lombok.Builder;
import lombok.Data;

import java.util.List;

@Data
@Builder
public class UpdateQaParam {
    private User user;
    private KnowledgeBaseQa qaRecord;
    private SseAskParam sseAskParam;
    @Nullable
    private List<ContentRetriever> retrievers;
    private String response;
    private int duration;
    private boolean isTokenFree;
}

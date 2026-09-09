package com.moyz.adi.common.vo;

import com.moyz.adi.common.entity.KnowledgeBaseAsk;
import com.moyz.adi.common.entity.User;
import dev.langchain4j.rag.content.retriever.ContentRetriever;
import jakarta.annotation.Nullable;
import lombok.Builder;
import lombok.Data;

import java.util.List;

@Data
@Builder
public class UpdateAskParam {
    private User user;
    private KnowledgeBaseAsk askRecord;
    private SseAskParam sseAskParam;
    @Nullable
    private List<ContentRetriever> retrievers;
    private String response;
    private int duration;
    private boolean isTokenFree;
}

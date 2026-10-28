package com.moyz.adi.common.vo;

import com.moyz.adi.common.entity.KnowledgeBaseMessage;
import com.moyz.adi.common.entity.User;
import dev.langchain4j.rag.content.retriever.ContentRetriever;
import jakarta.annotation.Nullable;
import lombok.Builder;
import lombok.Data;

import java.util.List;

@Data
@Builder
public class UpdateMessageParam {
    private User user;
    private KnowledgeBaseMessage chatRecord;
    private SseAskParam sseAskParam;
    @Nullable
    private List<ContentRetriever> retrievers;
    private String response;
    private int duration;
    private boolean isTokenFree;
}

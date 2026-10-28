package com.moyz.adi.common.vo;

import com.moyz.adi.common.entity.KnowledgeBaseChat;
import com.moyz.adi.common.entity.User;
import dev.langchain4j.rag.content.retriever.ContentRetriever;
import jakarta.annotation.Nullable;
import lombok.Builder;
import lombok.Data;

import java.util.List;

@Data
@Builder
public class UpdateChatParam {
    private User user;
    private KnowledgeBaseChat chatRecord;
    private SseAskParam sseAskParam;
    @Nullable
    private List<ContentRetriever> retrievers;
    private String response;
    private int duration;
    private boolean isTokenFree;
}

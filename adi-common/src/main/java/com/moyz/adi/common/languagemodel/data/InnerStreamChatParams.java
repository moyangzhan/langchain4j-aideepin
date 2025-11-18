package com.moyz.adi.common.languagemodel.data;

import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.interfaces.TriConsumer;
import com.moyz.adi.common.languagemodel.data.LLMResponseContent;
import com.moyz.adi.common.vo.AnswerMeta;
import com.moyz.adi.common.vo.PromptMeta;
import dev.langchain4j.mcp.client.McpClient;
import dev.langchain4j.model.chat.StreamingChatModel;
import dev.langchain4j.model.chat.request.ChatRequest;
import lombok.Builder;
import lombok.Data;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.util.List;

@Builder
@Data
public class InnerStreamChatParams {
    private String uuid;
    private SseEmitter sseEmitter;
    private List<McpClient> mcpClients;
    private StreamingChatModel streamingChatModel;
    private ChatRequest chatRequest;
    private Integer answerContentType;
    private TriConsumer<LLMResponseContent, PromptMeta, AnswerMeta> consumer;
    private User user;
}

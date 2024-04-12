package com.moyz.adi.common.vo;

import com.moyz.adi.common.entity.User;
import dev.langchain4j.memory.ChatMemory;
import lombok.Data;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;


@Data
public class SseAskParams {

    private String messageId;

    private User user;

    private String regenerateQuestionUuid;

    private String systemMessage;

    private ChatMemory chatMemory;

    private String userMessage;

    private SseEmitter sseEmitter;

    private String modelName;
}

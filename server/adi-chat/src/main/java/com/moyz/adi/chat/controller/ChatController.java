package com.moyz.adi.chat.controller;

import com.moyz.adi.common.dto.AskReq;
import com.moyz.adi.common.service.CharacterChatService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.annotation.Resource;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

/**
 * 聊天控制器：负责 SSE 聊天端点。
 * <p>
 * Chat controller: handles SSE chat endpoint.
 * </p>
 */
@Tag(name = "Chat")
@RestController
@RequestMapping("/chat")
@Validated
public class ChatController {

    @Resource
    private CharacterChatService characterChatService;

    @Operation(summary = "发送一个prompt给模型 | Send Prompt to Model")
    @PostMapping(value = "/process", produces = MediaType.TEXT_EVENT_STREAM_VALUE)
    public SseEmitter ask(@RequestBody @Validated AskReq askReq) {
        return characterChatService.sseAsk(askReq);
    }
}

package com.moyz.adi.chat.controller;

import com.moyz.adi.common.dto.AskReq;
import com.moyz.adi.common.dto.RefEmbeddingDto;
import com.moyz.adi.common.dto.RefGraphDto;
import com.moyz.adi.common.service.ConversationMessageRefEmbeddingService;
import com.moyz.adi.common.service.ConversationMessageRefGraphService;
import com.moyz.adi.common.service.ConversationMessageService;
import io.swagger.v3.oas.annotations.Operation;
import jakarta.annotation.Resource;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.util.List;

@RestController
@RequestMapping("/conversation/message")
@Validated
public class ConversationMessageController {

    @Resource
    private ConversationMessageService conversationMessageService;

    @Resource
    private ConversationMessageRefEmbeddingService conversationMessageRefEmbeddingService;

    @Resource
    private ConversationMessageRefGraphService conversationMessageRefGraphService;

    @Operation(summary = "发送一个prompt给模型")
    @PostMapping(value = "/process", produces = MediaType.TEXT_EVENT_STREAM_VALUE)
    public SseEmitter ask(@RequestBody @Validated AskReq askReq) {
        return conversationMessageService.sseAsk(askReq);
    }

    @Operation(summary = "根据音频uuid获取对应的文本")
    @GetMapping("/text/{audioUuid}")
    public String getTextByAudioUuid(@PathVariable String audioUuid) {
        return conversationMessageService.getTextByAudioUuid(audioUuid);
    }

    @GetMapping("/embedding-ref/{uuid}")
    public List<RefEmbeddingDto> embeddingRef(@PathVariable String uuid) {
        return conversationMessageRefEmbeddingService.listRefEmbeddings(uuid);
    }

    @GetMapping("/graph-ref/{uuid}")
    public RefGraphDto graphRef(@PathVariable String uuid) {
        return conversationMessageRefGraphService.getByMsgUuid(uuid);
    }

    @PostMapping("/del/{uuid}")
    public boolean softDelete(@PathVariable String uuid) {
        return conversationMessageService.softDelete(uuid);
    }

}

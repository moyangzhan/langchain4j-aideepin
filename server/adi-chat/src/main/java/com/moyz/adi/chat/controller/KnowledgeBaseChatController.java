package com.moyz.adi.chat.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.dto.*;
import com.moyz.adi.common.entity.KnowledgeBase;
import com.moyz.adi.common.service.KnowledgeBaseMessageRefEmbeddingService;
import com.moyz.adi.common.service.KnowledgeBaseMessageRefGraphService;
import com.moyz.adi.common.service.KnowledgeBaseChatService;
import com.moyz.adi.common.service.KnowledgeBaseService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.annotation.Resource;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.util.List;

@Tag(name = "知识库问答controller | Knowledge Base Chat Controller")
@RequestMapping("/knowledge-base/chat/")
@RestController
public class KnowledgeBaseChatController {

    @Resource
    private KnowledgeBaseService knowledgeBaseService;

    @Resource
    private KnowledgeBaseChatService knowledgeBaseChatService;

    @Resource
    private KnowledgeBaseMessageRefEmbeddingService knowledgeBaseMessageRefEmbeddingService;

    @Resource
    private KnowledgeBaseMessageRefGraphService knowledgeBaseMessageRefGraphService;

    @PostMapping("/add/{kbUuid}")
    public KbChatDto add(@PathVariable String kbUuid, @RequestBody @Validated KbChatReq req) {
        KnowledgeBase knowledgeBase = knowledgeBaseService.getReadableOrThrow(kbUuid);
        return knowledgeBaseChatService.add(knowledgeBase, req);
    }

    @Operation(summary = "流式响应 | SSE Response")
    @PostMapping(value = "/process/{chatRecordUuid}", produces = MediaType.TEXT_EVENT_STREAM_VALUE)
    public SseEmitter sseChat(@PathVariable String chatRecordUuid) {
        return knowledgeBaseService.sseChat(chatRecordUuid);
    }

    @GetMapping("/search")
    public Page<KbChatDto> list(String kbUuid, String keyword, @NotNull @Min(1) Integer currentPage, @NotNull @Min(10) Integer pageSize) {
        knowledgeBaseService.checkReadPrivilege(kbUuid);
        return knowledgeBaseChatService.search(kbUuid, keyword, currentPage, pageSize);
    }

    @PostMapping("/del/{uuid}")
    public boolean recordDel(@PathVariable String uuid) {
        return knowledgeBaseChatService.softDelete(uuid);
    }

    @GetMapping("/embedding-ref/{uuid}")
    public List<RefEmbeddingDto> embeddingRef(@PathVariable String uuid) {
        knowledgeBaseService.getReadableChatOrThrow(uuid);
        return knowledgeBaseMessageRefEmbeddingService.listRefEmbeddings(uuid);
    }

    @GetMapping("/graph-ref/{uuid}")
    public RefGraphDto graphRef(@PathVariable String uuid) {
        knowledgeBaseService.getReadableChatOrThrow(uuid);
        return knowledgeBaseMessageRefGraphService.getByChatUuid(uuid);
    }

    @PostMapping("/clear")
    public void recordDel() {
        knowledgeBaseChatService.clearByCurrentUser();
    }
}

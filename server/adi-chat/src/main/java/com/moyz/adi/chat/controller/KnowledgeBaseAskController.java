package com.moyz.adi.chat.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.dto.*;
import com.moyz.adi.common.entity.KnowledgeBase;
import com.moyz.adi.common.service.KnowledgeBaseAskRefEmbeddingService;
import com.moyz.adi.common.service.KnowledgeBaseAskRefGraphService;
import com.moyz.adi.common.service.KnowledgeBaseAskService;
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

@Tag(name = "知识库问答controller | Knowledge Base QA Controller")
@RequestMapping("/knowledge-base/qa/")
@RestController
public class KnowledgeBaseAskController {

    @Resource
    private KnowledgeBaseService knowledgeBaseService;

    @Resource
    private KnowledgeBaseAskService knowledgeBaseAskService;

    @Resource
    private KnowledgeBaseAskRefEmbeddingService knowledgeBaseAskRefEmbeddingService;

    @Resource
    private KnowledgeBaseAskRefGraphService knowledgeBaseAskRefGraphService;

    @PostMapping("/add/{kbUuid}")
    public KbAskDto add(@PathVariable String kbUuid, @RequestBody @Validated KbAskReq req) {
        KnowledgeBase knowledgeBase = knowledgeBaseService.getReadableOrThrow(kbUuid);
        return knowledgeBaseAskService.add(knowledgeBase, req);
    }

    @Operation(summary = "流式响应 | SSE Response")
    @PostMapping(value = "/process/{askRecordUuid}", produces = MediaType.TEXT_EVENT_STREAM_VALUE)
    public SseEmitter sseAsk(@PathVariable String askRecordUuid) {
        return knowledgeBaseService.sseAsk(askRecordUuid);
    }

    @GetMapping("/search")
    public Page<KbAskDto> list(String kbUuid, String keyword, @NotNull @Min(1) Integer currentPage, @NotNull @Min(10) Integer pageSize) {
        knowledgeBaseService.checkReadPrivilege(kbUuid);
        return knowledgeBaseAskService.search(kbUuid, keyword, currentPage, pageSize);
    }

    @PostMapping("/del/{uuid}")
    public boolean recordDel(@PathVariable String uuid) {
        return knowledgeBaseAskService.softDelete(uuid);
    }

    @GetMapping("/embedding-ref/{uuid}")
    public List<RefEmbeddingDto> embeddingRef(@PathVariable String uuid) {
        knowledgeBaseService.getReadableAskOrThrow(uuid);
        return knowledgeBaseAskRefEmbeddingService.listRefEmbeddings(uuid);
    }

    @GetMapping("/graph-ref/{uuid}")
    public RefGraphDto graphRef(@PathVariable String uuid) {
        knowledgeBaseService.getReadableAskOrThrow(uuid);
        return knowledgeBaseAskRefGraphService.getByAskUuid(uuid);
    }

    @PostMapping("/clear")
    public void recordDel() {
        knowledgeBaseAskService.clearByCurrentUser();
    }
}

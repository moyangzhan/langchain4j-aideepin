package com.moyz.adi.chat.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.dto.*;
import com.moyz.adi.common.entity.KnowledgeBase;
import com.moyz.adi.common.service.KnowledgeBaseQaRecordReferenceService;
import com.moyz.adi.common.service.KnowledgeBaseQaRefGraphService;
import com.moyz.adi.common.service.KnowledgeBaseQaService;
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

@Tag(name = "知识库问答controller")
@RequestMapping("/knowledge-base/qa/")
@RestController
public class KnowledgeBaseQAController {

    @Resource
    private KnowledgeBaseService knowledgeBaseService;

    @Resource
    private KnowledgeBaseQaService knowledgeBaseQaService;

    @Resource
    private KnowledgeBaseQaRecordReferenceService knowledgeBaseQaRecordReferenceService;

    @Resource
    private KnowledgeBaseQaRefGraphService knowledgeBaseQaRefGraphService;

    @PostMapping("/add/{kbUuid}")
    public KbQaDto add(@PathVariable String kbUuid, @RequestBody @Validated QARecordReq req) {
        KnowledgeBase knowledgeBase = knowledgeBaseService.getOrThrow(kbUuid);
        return knowledgeBaseQaService.add(knowledgeBase, req);
    }

    @Operation(summary = "流式响应")
    @PostMapping(value = "/process/{qaRecordUuid}", produces = MediaType.TEXT_EVENT_STREAM_VALUE)
    public SseEmitter sseAsk(@PathVariable String qaRecordUuid) {
        return knowledgeBaseService.sseAsk(qaRecordUuid);
    }

    @GetMapping("/search")
    public Page<KbQaDto> list(String kbUuid, String keyword, @NotNull @Min(1) Integer currentPage, @NotNull @Min(10) Integer pageSize) {
        return knowledgeBaseQaService.search(kbUuid, keyword, currentPage, pageSize);
    }

    @PostMapping("/del/{uuid}")
    public boolean recordDel(@PathVariable String uuid) {
        return knowledgeBaseQaService.softDelete(uuid);
    }

    @GetMapping("/embedding-ref/{uuid}")
    public List<RefEmbeddingDto> embeddingRef(@PathVariable String uuid) {
        return knowledgeBaseQaRecordReferenceService.listRefEmbeddings(uuid);
    }

    @GetMapping("/graph-ref/{uuid}")
    public RefGraphDto graphRef(@PathVariable String uuid) {
        return knowledgeBaseQaRefGraphService.getByQaUuid(uuid);
    }

    @PostMapping("/clear")
    public void recordDel() {
        knowledgeBaseQaService.clearByCurrentUser();
    }
}

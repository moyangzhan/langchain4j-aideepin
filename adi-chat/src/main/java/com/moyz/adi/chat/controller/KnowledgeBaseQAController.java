package com.moyz.adi.chat.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.dto.KbQaRecordDto;
import com.moyz.adi.common.dto.QAReq;
import com.moyz.adi.common.service.KnowledgeBaseQaRecordService;
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

@Tag(name = "知识库问答controller")
@RequestMapping("/knowledge-base/qa/")
@RestController
public class KnowledgeBaseQAController {

    @Resource
    private KnowledgeBaseService knowledgeBaseService;

    @Resource
    private KnowledgeBaseQaRecordService knowledgeBaseQaRecordService;

    @Operation(summary = "流式响应")
    @PostMapping(value = "/process/{kbUuid}", produces = MediaType.TEXT_EVENT_STREAM_VALUE)
    public SseEmitter sseAsk(@PathVariable String kbUuid, @RequestBody @Validated QAReq req) {
        return knowledgeBaseService.sseAsk(kbUuid, req);
    }

    @GetMapping("/record/search")
    public Page<KbQaRecordDto> list(String kbUuid, String keyword, @NotNull @Min(1) Integer currentPage, @NotNull @Min(10) Integer pageSize) {
        return knowledgeBaseQaRecordService.search(kbUuid, keyword, currentPage, pageSize);
    }

    @PostMapping("/record/del/{uuid}")
    public boolean recordDel(@PathVariable String uuid) {
        return knowledgeBaseQaRecordService.softDelete(uuid);
    }
}

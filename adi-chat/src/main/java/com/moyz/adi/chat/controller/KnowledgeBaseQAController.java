package com.moyz.adi.chat.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.dto.KbQaRecordDto;
import com.moyz.adi.common.dto.KbQaRecordReferenceDto;
import com.moyz.adi.common.dto.QARecordReq;
import com.moyz.adi.common.dto.QAReq;
import com.moyz.adi.common.entity.KnowledgeBase;
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

import java.util.List;

@Tag(name = "知识库问答controller")
@RequestMapping("/knowledge-base/qa/")
@RestController
public class KnowledgeBaseQAController {

    @Resource
    private KnowledgeBaseService knowledgeBaseService;

    @Resource
    private KnowledgeBaseQaRecordService knowledgeBaseQaRecordService;

    @PostMapping("/record/add/{kbUuid}")
    public KbQaRecordDto add(@PathVariable String kbUuid, @RequestBody @Validated QARecordReq req) {
        KnowledgeBase knowledgeBase = knowledgeBaseService.getOrThrow(kbUuid);
        return knowledgeBaseQaRecordService.add(knowledgeBase, req);
    }

    @Operation(summary = "流式响应")
    @PostMapping(value = "/process/{qaRecordUuid}", produces = MediaType.TEXT_EVENT_STREAM_VALUE)
    public SseEmitter sseAsk(@PathVariable String qaRecordUuid) {
        return knowledgeBaseService.sseAsk(qaRecordUuid);
    }

    @GetMapping("/record/search")
    public Page<KbQaRecordDto> list(String kbUuid, String keyword, @NotNull @Min(1) Integer currentPage, @NotNull @Min(10) Integer pageSize) {
        return knowledgeBaseQaRecordService.search(kbUuid, keyword, currentPage, pageSize);
    }

    @PostMapping("/record/del/{uuid}")
    public boolean recordDel(@PathVariable String uuid) {
        return knowledgeBaseQaRecordService.softDelete(uuid);
    }

    @GetMapping("/record/reference/{uuid}")
    public List<KbQaRecordReferenceDto> reference(@PathVariable String uuid) {
        return knowledgeBaseQaRecordService.listReferences(uuid);
    }

    @PostMapping("/record/clear")
    public void recordDel() {
        knowledgeBaseQaRecordService.clearByCurrentUser();
    }
}

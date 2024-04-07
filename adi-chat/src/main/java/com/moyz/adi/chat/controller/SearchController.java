package com.moyz.adi.chat.controller;

import com.moyz.adi.common.dto.AiSearchReq;
import com.moyz.adi.common.service.SearchService;
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

@Tag(name = "AI search controller")
@RequestMapping("/ai-search/")
@RestController
public class SearchController {

    @Resource
    private SearchService searchService;

    @Operation(summary = "sse process")
    @PostMapping(value = "/process", produces = MediaType.TEXT_EVENT_STREAM_VALUE)
    public SseEmitter sseAsk(@RequestBody @Validated AiSearchReq req) {
        return searchService.search(req.isBriefSearch(), req.getSearchText(), req.getEngineName(), req.getModelName());
    }
}

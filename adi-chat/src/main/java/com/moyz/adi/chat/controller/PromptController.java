package com.moyz.adi.chat.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.*;
import com.moyz.adi.common.service.PromptService;
import io.swagger.v3.oas.annotations.Operation;
import jakarta.annotation.Resource;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/prompt")
@Validated
public class PromptController {

    @Resource
    private PromptService promptService;

    @Operation(summary = "查询列表")
    @GetMapping(value = "/my/all")
    public List<PromptDto> myAll() {
        return promptService.getAll(ThreadContext.getCurrentUserId());
    }

    @Operation(summary = "查询列表")
    @GetMapping(value = "/my/listByUpdateTime")
    public PromptListResp list(@RequestParam(required = false) LocalDateTime minUpdateTime) {
        return promptService.listByMinUpdateTime(minUpdateTime);
    }

    @Operation(summary = "搜索列表")
    @GetMapping(value = "/my/search")
    public Page<PromptDto> search(String keyword, @NotNull @Min(1) Integer currentPage, @NotNull @Min(10) Integer pageSize) {
        return promptService.search(keyword, currentPage, pageSize);
    }

    @Operation(summary = "自动填充列表")
    @GetMapping(value = "/my/autocomplete")
    public List<PromptDto> autocomplete(String keyword) {
        return promptService.autocomplete(keyword);
    }

    @Operation(summary = "保存列表")
    @PostMapping(value = "/save")
    public Map<String, Long> savePrompts(@RequestBody PromptsSaveReq savePromptsReq) {
        return promptService.savePrompts(savePromptsReq);
    }

    @Operation(summary = "删除")
    @PostMapping(value = "/del/{id}")
    public boolean softDelete(@PathVariable Long id) {
        return promptService.softDelete(id);
    }

    @Operation(summary = "编辑")
    @PostMapping(value = "/edit/{id}")
    public boolean edit(@PathVariable Long id, @RequestBody PromptEditReq promptEditReq) {
        return promptService.edit(id, promptEditReq.getTitle(), promptEditReq.getRemark());
    }

    @Operation(summary = "search")
    @GetMapping(value = "/search")
    public List<PromptDto> search(@Validated SearchReq searchReq) {
        return promptService.search(searchReq.getKeyword());
    }
}

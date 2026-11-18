package com.moyz.adi.admin.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.dto.KbDocumentBaseEditReq;
import com.moyz.adi.common.dto.KbDocumentDto;
import com.moyz.adi.common.service.KbDocumentService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.annotation.Resource;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

/**
 * Knowledge Base Document
 */
@Tag(name = "知识库文档管理 | Knowledge Base Document Management", description = "知识库文档管理 | Knowledge Base Document Management")
@RestController
@RequestMapping("/admin/document")
@Validated
public class AdminDocumentController {

    @Resource
    private KbDocumentService kbDocumentService;

    @Operation(summary = "搜索知识库文档 | Search Documents")
    @GetMapping("/search")
    public Page<KbDocumentDto> search(String kbUuid, String keyword,
                                      @NotNull @Min(1) Integer currentPage, @NotNull @Min(1) @Max(100) Integer pageSize) {
        return kbDocumentService.search(kbUuid, keyword, currentPage, pageSize);
    }

    @Operation(summary = "删除文档 | Delete Document")
    @PostMapping("/del/{uuid}")
    public boolean delete(@PathVariable String uuid) {
        return kbDocumentService.softDelete(uuid);
    }

    @Operation(summary = "编辑文档基础信息 | Edit Document Base Info")
    @PostMapping("/edit")
    public boolean edit(@RequestBody @Valid KbDocumentBaseEditReq req) {
        return kbDocumentService.updateBaseInfo(req);
    }
}

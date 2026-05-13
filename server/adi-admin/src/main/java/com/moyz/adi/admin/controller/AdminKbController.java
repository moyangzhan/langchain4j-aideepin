package com.moyz.adi.admin.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.dto.KbEditReq;
import com.moyz.adi.common.dto.KbInfoResp;
import com.moyz.adi.common.dto.KbSearchReq;
import com.moyz.adi.common.service.KnowledgeBaseService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.annotation.Resource;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

/**
 * Knowledge Base
 */
@Tag(name = "知识库管理 | Knowledge Base Management", description = "知识库管理 | Knowledge Base Management")
@RestController
@RequestMapping("/admin/kb")
@Validated
public class AdminKbController {

    @Resource
    private KnowledgeBaseService knowledgeBaseService;

    @Operation(summary = "搜索知识库 | Search Knowledge Bases")
    @PostMapping("/search")
    public Page<KbInfoResp> search(@RequestBody KbSearchReq kbSearchReq, @NotNull @Min(1) Integer currentPage, @NotNull @Min(10) Integer pageSize) {
        return knowledgeBaseService.search(kbSearchReq, currentPage, pageSize);
    }

    @Operation(summary = "删除知识库 | Delete Knowledge Base")
    @PostMapping("/del/{uuid}")
    public boolean delete(@PathVariable String uuid) {
        return knowledgeBaseService.softDelete(uuid);
    }

    @Operation(summary = "编辑知识库 | Edit Knowledge Base")
    @PostMapping("/edit")
    public boolean edit(@RequestBody KbEditReq kbEditReq) {
        knowledgeBaseService.saveOrUpdate(kbEditReq);
        return true;
    }
}

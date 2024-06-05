package com.moyz.adi.admin.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.dto.KbEditReq;
import com.moyz.adi.common.dto.KbInfoResp;
import com.moyz.adi.common.dto.KbSearchReq;
import com.moyz.adi.common.service.KnowledgeBaseService;
import jakarta.annotation.Resource;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

/**
 * Knowledge Base
 */
@RestController
@RequestMapping("/admin/kb")
@Validated
public class AdminKbController {

    @Resource
    private KnowledgeBaseService knowledgeBaseService;

    @PostMapping("/search")
    public Page<KbInfoResp> search(@RequestBody KbSearchReq kbSearchReq, @NotNull @Min(1) Integer currentPage, @NotNull @Min(10) Integer pageSize) {
        return knowledgeBaseService.search(kbSearchReq, currentPage, pageSize);
    }

    @PostMapping("/delete/{uuid}")
    public boolean delete(@PathVariable String uuid) {
        return knowledgeBaseService.softDelete(uuid);
    }

    @PostMapping("/edit")
    public boolean edit(@RequestBody KbEditReq kbEditReq) {
        return knowledgeBaseService.updateKb(kbEditReq);
    }
}

package com.moyz.adi.chat.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.dto.KbItemEditReq;
import com.moyz.adi.common.dto.KbItemEmbeddingBatchReq;
import com.moyz.adi.common.entity.KnowledgeBaseItem;
import com.moyz.adi.common.service.KnowledgeBaseItemService;
import jakarta.annotation.Resource;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/knowledge-base-item")
@Validated
public class KnowledgeBaseItemController {

    @Resource
    private KnowledgeBaseItemService knowledgeBaseItemService;

    @PostMapping("/saveOrUpdate")
    public KnowledgeBaseItem saveOrUpdate(@RequestBody KbItemEditReq itemEditReq) {
        return knowledgeBaseItemService.saveOrUpdate(itemEditReq);
    }

    @GetMapping("/search")
    public Page<KnowledgeBaseItem> search(String kbUuid, String keyword, @NotNull @Min(1) Integer currentPage, @NotNull @Min(10) Integer pageSize) {
        return knowledgeBaseItemService.search(kbUuid, keyword, currentPage, pageSize);
    }

    @GetMapping("/info/{uuid}")
    public KnowledgeBaseItem info(@PathVariable String uuid) {
        return knowledgeBaseItemService.lambdaQuery()
                .eq(KnowledgeBaseItem::getUuid, uuid)
                .eq(KnowledgeBaseItem::getIsDeleted, false)
                .one();
    }

    @PostMapping("/embedding/{uuid}")
    public boolean embedding(@PathVariable String uuid) {
        return knowledgeBaseItemService.checkAndEmbedding(uuid);
    }

    @PostMapping("/embedding-list")
    public boolean embeddingBatch(@RequestBody KbItemEmbeddingBatchReq req) {
        return knowledgeBaseItemService.checkAndEmbedding(req.getUuids());
    }

    @PostMapping("/del/{uuid}")
    public boolean softDelete(@PathVariable String uuid) {
        return knowledgeBaseItemService.softDelete(uuid);
    }
}

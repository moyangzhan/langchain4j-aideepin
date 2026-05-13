package com.moyz.adi.chat.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.dto.KbItemDto;
import com.moyz.adi.common.dto.KbItemEditReq;
import com.moyz.adi.common.entity.KnowledgeBaseItem;
import com.moyz.adi.common.service.KnowledgeBaseItemService;
import jakarta.annotation.Resource;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

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
    public Page<KbItemDto> search(String kbUuid, String keyword, @NotNull @Min(1) Integer currentPage, @NotNull @Min(10) Integer pageSize) {
        return knowledgeBaseItemService.search(kbUuid, keyword, currentPage, pageSize);
    }

    @GetMapping("/info/{uuid}")
    public KnowledgeBaseItem info(@PathVariable String uuid) {
        return knowledgeBaseItemService.lambdaQuery()
                .eq(KnowledgeBaseItem::getUuid, uuid)
                .eq(KnowledgeBaseItem::getIsDeleted, false)
                .one();
    }

    @PostMapping("/del/{uuid}")
    public boolean softDelete(@PathVariable String uuid) {
        return knowledgeBaseItemService.softDelete(uuid);
    }
}

package com.moyz.adi.chat.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.KbEditReq;
import com.moyz.adi.common.dto.KbInfoResp;
import com.moyz.adi.common.dto.KbSearchReq;
import com.moyz.adi.common.entity.AdiFile;
import com.moyz.adi.common.entity.KnowledgeBase;
import com.moyz.adi.common.service.KnowledgeBaseService;
import jakarta.annotation.Resource;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

@RestController
@RequestMapping("/knowledge-base")
@Validated
public class KnowledgeBaseController {

    @Resource
    private KnowledgeBaseService knowledgeBaseService;

    @PostMapping("/saveOrUpdate")
    public KnowledgeBase saveOrUpdate(@RequestBody KbEditReq kbEditReq) {
        return knowledgeBaseService.saveOrUpdate(kbEditReq);
    }

    @PostMapping(path = "/uploadDocs/{uuid}", headers = "content-type=multipart/form-data", produces = MediaType.APPLICATION_JSON_VALUE)
    public boolean uploadDocs(@PathVariable String uuid, @RequestParam(value = "embedding", defaultValue = "true") Boolean embedding, @RequestParam("files") MultipartFile[] docs) {
        knowledgeBaseService.uploadDocs(uuid, embedding, docs);
        return true;
    }

    @PostMapping(path = "/upload/{uuid}", headers = "content-type=multipart/form-data", produces = MediaType.APPLICATION_JSON_VALUE)
    public AdiFile upload(@PathVariable String uuid, @RequestParam(value = "embedding", defaultValue = "true") Boolean embedding, @RequestParam("file") MultipartFile doc) {
        return knowledgeBaseService.uploadDoc(uuid, embedding, doc);
    }

    @GetMapping("/searchMine")
    public Page<KbInfoResp> searchMine(@RequestParam(defaultValue = "") String keyword, @RequestParam(defaultValue = "false") Boolean includeOthersPublic, @NotNull @Min(1) Integer currentPage, @NotNull @Min(10) Integer pageSize) {
        return knowledgeBaseService.searchMine(keyword, includeOthersPublic, currentPage, pageSize);
    }

    @GetMapping("/searchPublic")
    public Page<KbInfoResp> searchPublic(@RequestParam(defaultValue = "") String keyword, @NotNull @Min(1) Integer currentPage, @NotNull @Min(10) Integer pageSize) {
        return knowledgeBaseService.search(KbSearchReq.builder().isPublic(true).title(keyword).build(), currentPage, pageSize);
    }

    @GetMapping("/info/{uuid}")
    public KnowledgeBase info(@PathVariable String uuid) {
        return knowledgeBaseService.lambdaQuery()
                .eq(KnowledgeBase::getUuid, uuid)
                .eq(KnowledgeBase::getIsDeleted, false)
                .one();
    }

    @PostMapping("/del/{uuid}")
    public boolean softDelete(@PathVariable String uuid) {
        return knowledgeBaseService.softDelete(uuid);
    }

    @PostMapping("/embedding/{uuid}")
    public boolean embedding(@PathVariable String uuid, @RequestParam(defaultValue = "false") Boolean forceAll) {
        return knowledgeBaseService.embedding(uuid, forceAll);
    }

    /**
     * 点赞
     *
     * @return
     */
    @PostMapping("/star/toggle")
    public boolean star(@RequestParam @NotBlank String kbUuid) {
        return knowledgeBaseService.toggleStar(ThreadContext.getCurrentUser(), kbUuid);
    }
}

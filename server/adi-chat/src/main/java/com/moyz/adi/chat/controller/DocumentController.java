package com.moyz.adi.chat.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.dto.KbDocumentDto;
import com.moyz.adi.common.dto.KbDocumentEditReq;
import com.moyz.adi.common.dto.KbDocumentToggleStatusReq;
import com.moyz.adi.common.entity.KbDocument;
import com.moyz.adi.common.service.DocumentQaService;
import com.moyz.adi.common.service.KbDocumentService;
import com.moyz.adi.common.service.KnowledgeBaseService;
import jakarta.annotation.Resource;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.nio.charset.StandardCharsets;

@RestController
@RequestMapping("/document")
@Validated
public class DocumentController {

    @Resource
    private KbDocumentService kbDocumentService;

    @Resource
    private KnowledgeBaseService knowledgeBaseService;

    @Resource
    private DocumentQaService documentQaService;

    @PostMapping("/saveOrUpdate")
    public KbDocument saveOrUpdate(@RequestBody KbDocumentEditReq itemEditReq) {
        return kbDocumentService.saveOrUpdate(itemEditReq);
    }

    @GetMapping("/search")
    public Page<KbDocumentDto> search(String kbUuid, String keyword, @NotNull @Min(1) Integer currentPage, @NotNull @Min(10) Integer pageSize) {
        knowledgeBaseService.checkReadPrivilege(kbUuid);
        return kbDocumentService.search(kbUuid, keyword, currentPage, pageSize);
    }

    @GetMapping("/info/{uuid}")
    public KbDocument info(@PathVariable String uuid) {
        return kbDocumentService.info(uuid);
    }

    @PostMapping("/del/{uuid}")
    public boolean softDelete(@PathVariable String uuid) {
        return kbDocumentService.softDelete(uuid);
    }

    @PostMapping("/toggle-status")
    public boolean toggleStatus(@RequestBody KbDocumentToggleStatusReq req) {
        return kbDocumentService.toggleStatus(req.getUuid(), req.getIsEnabled());
    }

    /**
     * 批量导入 QA 对（Dify 格式 xlsx/csv：首行表头 question,answer，其后每行一对），
     * 生成 segment_mode=qa 的文档并触发向量化
     */
    @PostMapping("/uploadQa/{kbUuid}")
    public KbDocument uploadQa(@PathVariable String kbUuid, @RequestParam("file") MultipartFile file) {
        return knowledgeBaseService.uploadQa(kbUuid, file);
    }

    /**
     * QA 导入模板下载
     */
    @GetMapping("/qaImportTemplate")
    public ResponseEntity<byte[]> qaImportTemplate() {
        byte[] body = DocumentQaService.QA_IMPORT_TEMPLATE_CSV.getBytes(StandardCharsets.UTF_8);
        return ResponseEntity.ok()
                .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=qa_import_template.csv")
                .contentType(MediaType.parseMediaType("text/csv;charset=UTF-8"))
                .body(body);
    }
}

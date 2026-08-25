package com.moyz.adi.chat.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.KbDocumentDto;
import com.moyz.adi.common.dto.KbDocumentEditReq;
import com.moyz.adi.common.dto.KbDocumentToggleStatusReq;
import com.moyz.adi.common.entity.KbDocument;
import com.moyz.adi.common.enums.SegmentModeEnum;
import com.moyz.adi.common.exception.BaseException;
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

import static com.moyz.adi.common.enums.ErrorEnum.A_DATA_NOT_FOUND;
import static com.moyz.adi.common.enums.ErrorEnum.A_PARAMS_ERROR;

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
     * 失败重试：qa 生成失败走重新生成，索引失败按维度重新入队（服务端路由，前端不感知内部规则）
     */
    @PostMapping("/retryIndex/{uuid}")
    public boolean retryIndex(@PathVariable String uuid) {
        return kbDocumentService.retryIndex(uuid);
    }

    /**
     * （重新）生成问答对：无段行直接生成；已有问答对替换式重新生成（服务端守卫在跑/生成中并清理重建）
     */
    @PostMapping("/autoGenerateQa/{uuid}")
    public boolean autoGenerateQa(@PathVariable String uuid) {
        kbDocumentService.checkWritePrivilege(uuid);
        KbDocument doc = kbDocumentService.getEnable(uuid);
        if (null == doc) {
            throw new BaseException(A_DATA_NOT_FOUND);
        }
        if (SegmentModeEnum.QA != doc.getSegmentMode()) {
            throw new BaseException(A_PARAMS_ERROR);
        }
        documentQaService.autoGenerateQa(ThreadContext.getCurrentUser(), doc);
        return true;
    }

    /**
     * 导入问答对到已有文档（追加：相同答案并入既有段、问题去重；服务端守卫生成中/在跑任务）
     */
    @PostMapping("/importQa/{uuid}")
    public boolean importQa(@PathVariable String uuid, @RequestParam("file") MultipartFile file) {
        kbDocumentService.checkWritePrivilege(uuid);
        KbDocument doc = kbDocumentService.getEnable(uuid);
        if (null == doc) {
            throw new BaseException(A_DATA_NOT_FOUND);
        }
        if (SegmentModeEnum.QA != doc.getSegmentMode()) {
            throw new BaseException(A_PARAMS_ERROR);
        }
        documentQaService.importQaToDocument(doc, file);
        return true;
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

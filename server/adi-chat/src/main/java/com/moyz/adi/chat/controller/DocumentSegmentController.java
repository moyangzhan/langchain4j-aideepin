package com.moyz.adi.chat.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.dto.DocumentSegmentChildChunkEditReq;
import com.moyz.adi.common.dto.DocumentSegmentChildChunkRegenerateReq;
import com.moyz.adi.common.dto.DocumentSegmentDto;
import com.moyz.adi.common.dto.DocumentSegmentEditReq;
import com.moyz.adi.common.dto.DocumentSegmentQuestionEditReq;
import com.moyz.adi.common.dto.DocumentSegmentToggleStatusReq;
import com.moyz.adi.common.dto.QaPairEditReq;
import com.moyz.adi.common.entity.DocumentSegmentChildChunk;
import com.moyz.adi.common.entity.DocumentSegmentQuestion;
import com.moyz.adi.common.entity.KbDocument;
import com.moyz.adi.common.entity.KnowledgeBase;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.service.DocumentSegmentManageService;
import com.moyz.adi.common.service.KbDocumentService;
import com.moyz.adi.common.service.KnowledgeBaseService;
import jakarta.annotation.Resource;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import static com.moyz.adi.common.enums.ErrorEnum.A_DATA_NOT_FOUND;

/**
 * 分段管理（模式感知）：列表 / 编辑 / 删除。
 * 编辑 text 段、问题、子块会重新向量化；编辑答案、父段仅更新关系行。
 */
@RestController
@RequestMapping("/document-segment")
@Validated
public class DocumentSegmentController {

    @Resource
    private DocumentSegmentManageService documentSegmentManageService;

    @Resource
    private KbDocumentService kbDocumentService;

    @Resource
    private KnowledgeBaseService knowledgeBaseService;

    @GetMapping("/list/{docUuid}")
    public Page<DocumentSegmentDto> list(@PathVariable String docUuid,
                                         @NotNull @Min(1) Integer currentPage,
                                         @NotNull @Min(1) Integer pageSize) {
        kbDocumentService.checkReadPrivilege(docUuid);
        return documentSegmentManageService.list(docUuid, currentPage, pageSize);
    }

    /**
     * 编辑主表段内容（text 段文本 / qa 答案 / parent_child 父段）
     */
    @PostMapping("/saveOrUpdate")
    public boolean saveOrUpdate(@RequestBody @Validated DocumentSegmentEditReq req) {
        kbDocumentService.checkWritePrivilege(req.getDocUuid());
        return documentSegmentManageService.editSegment(req);
    }

    /**
     * 新增/编辑 QA 问题（支持把新问题挂到已有答案）
     */
    @PostMapping("/question/saveOrUpdate")
    public DocumentSegmentQuestion saveOrUpdateQuestion(@RequestBody @Validated DocumentSegmentQuestionEditReq req) {
        kbDocumentService.checkWritePrivilege(req.getDocUuid());
        KbDocument doc = kbDocumentService.getEnable(req.getDocUuid());
        // admins bypass the privilege probe without an existence check: a bogus/deleted uuid
        // must 404 instead of NPE
        if (doc == null) {
            throw new BaseException(A_DATA_NOT_FOUND);
        }
        KnowledgeBase kb = knowledgeBaseService.getOrThrow(doc.getKbUuid());
        return documentSegmentManageService.saveOrUpdateQuestion(doc, kb, req);
    }

    /**
     * 新增/编辑子块
     */
    @PostMapping("/child/saveOrUpdate")
    public DocumentSegmentChildChunk saveOrUpdateChild(@RequestBody @Validated DocumentSegmentChildChunkEditReq req) {
        kbDocumentService.checkWritePrivilege(req.getDocUuid());
        KbDocument doc = kbDocumentService.getEnable(req.getDocUuid());
        // admins bypass the privilege probe without an existence check: a bogus/deleted uuid
        // must 404 instead of NPE
        if (doc == null) {
            throw new BaseException(A_DATA_NOT_FOUND);
        }
        KnowledgeBase kb = knowledgeBaseService.getOrThrow(doc.getKbUuid());
        return documentSegmentManageService.saveOrUpdateChildChunk(doc, kb, req);
    }

    /**
     * Re-split child chunks from the parent segment's current content and rebuild vectors
     */
    @PostMapping("/child/regenerate")
    public boolean regenerateChild(@RequestBody @Validated DocumentSegmentChildChunkRegenerateReq req) {
        kbDocumentService.checkWritePrivilege(req.getDocUuid());
        KbDocument doc = kbDocumentService.getEnable(req.getDocUuid());
        // admins bypass the privilege probe without an existence check: a bogus/deleted uuid
        // must 404 instead of NPE
        if (doc == null) {
            throw new BaseException(A_DATA_NOT_FOUND);
        }
        KnowledgeBase kb = knowledgeBaseService.getOrThrow(doc.getKbUuid());
        return documentSegmentManageService.regenerateChildChunks(doc, kb, req);
    }

    /**
     * Edit a QA pair: answer plus the full question set; questions are content-diffed server-side
     */
    @PostMapping("/qaPair/saveOrUpdate")
    public boolean saveOrUpdateQaPair(@RequestBody @Validated QaPairEditReq req) {
        kbDocumentService.checkWritePrivilege(req.getDocUuid());
        KbDocument doc = kbDocumentService.getEnable(req.getDocUuid());
        // admins bypass the privilege probe without an existence check: a bogus/deleted uuid
        // must 404 instead of NPE
        if (doc == null) {
            throw new BaseException(A_DATA_NOT_FOUND);
        }
        KnowledgeBase kb = knowledgeBaseService.getOrThrow(doc.getKbUuid());
        return documentSegmentManageService.editQaPair(doc, kb, req);
    }

    /**
     * Repair vector drift of one segment: null out embedding ids missing from the store
     * and enqueue a re-embedding task
     */
    @PostMapping("/repairVector/{uuid}")
    public boolean repairVector(@PathVariable @NotBlank String uuid) {
        assertWritePrivilegeBySegment(uuid);
        return documentSegmentManageService.repairSegmentVector(uuid);
    }

    @PostMapping("/del/{uuid}")
    public boolean del(@PathVariable @NotBlank String uuid) {
        assertWritePrivilegeBySegment(uuid);
        return documentSegmentManageService.deleteSegment(uuid);
    }

    /**
     * 分段启停：停用删除该段向量与图谱足迹；启用重嵌向量并异步重抽图谱（消耗模型额度）
     */
    @PostMapping("/toggle-status")
    public boolean toggleStatus(@RequestBody @Validated DocumentSegmentToggleStatusReq req) {
        assertWritePrivilegeBySegment(req.getUuid());
        return documentSegmentManageService.toggleStatus(req.getUuid(), req.getIsEnabled());
    }

    @PostMapping("/question/del/{uuid}")
    public boolean delQuestion(@PathVariable @NotBlank String uuid) {
        assertWritePrivilegeByQuestion(uuid);
        return documentSegmentManageService.deleteQuestion(uuid);
    }

    @PostMapping("/child/del/{uuid}")
    public boolean delChild(@PathVariable @NotBlank String uuid) {
        assertWritePrivilegeByChild(uuid);
        return documentSegmentManageService.deleteChildChunk(uuid);
    }

    private void assertWritePrivilegeBySegment(String uuid) {
        com.moyz.adi.common.entity.DocumentSegment segment = documentSegmentManageService.getDocumentByUuid(uuid);
        kbDocumentService.checkWritePrivilege(segment.getDocUuid());
    }

    private void assertWritePrivilegeByQuestion(String uuid) {
        DocumentSegmentQuestion question = documentSegmentManageService.getQuestionByUuid(uuid);
        kbDocumentService.checkWritePrivilege(question.getDocUuid());
    }

    private void assertWritePrivilegeByChild(String uuid) {
        DocumentSegmentChildChunk child = documentSegmentManageService.getChildByUuid(uuid);
        kbDocumentService.checkWritePrivilege(child.getDocUuid());
    }
}

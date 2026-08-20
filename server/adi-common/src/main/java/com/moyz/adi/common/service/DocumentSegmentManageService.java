package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.DocumentSegmentChildChunkDto;
import com.moyz.adi.common.dto.DocumentSegmentChildChunkEditReq;
import com.moyz.adi.common.dto.DocumentSegmentDto;
import com.moyz.adi.common.dto.DocumentSegmentEditReq;
import com.moyz.adi.common.dto.DocumentSegmentQuestionDto;
import com.moyz.adi.common.dto.DocumentSegmentQuestionEditReq;
import com.moyz.adi.common.entity.DocumentSegment;
import com.moyz.adi.common.entity.DocumentSegmentChildChunk;
import com.moyz.adi.common.entity.DocumentSegmentQuestion;
import com.moyz.adi.common.entity.KbDocument;
import com.moyz.adi.common.entity.KnowledgeBase;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.enums.EmbeddingStatusEnum;
import com.moyz.adi.common.enums.GraphicalStatusEnum;
import com.moyz.adi.common.enums.SegmentModeEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.service.embedding.IKnowledgeEmbeddingService;
import com.moyz.adi.common.util.UuidUtil;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import com.moyz.adi.common.cosntant.AdiConstant;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static com.moyz.adi.common.enums.ErrorEnum.A_DATA_NOT_FOUND;
import static com.moyz.adi.common.enums.ErrorEnum.A_DOC_INDEX_DOING;
import static com.moyz.adi.common.enums.ErrorEnum.A_PARAMS_ERROR;

/**
 * 分段管理：模式感知列表 + 段/问题/子块的编辑与删除。
 * 编辑 text 段/问题/子块内容会重新向量化该条；编辑答案/父段仅更新关系行。
 */
@Slf4j
@Service
public class DocumentSegmentManageService {

    @Resource
    private DocumentSegmentService documentSegmentService;

    @Resource
    private DocumentSegmentQuestionService questionService;

    @Resource
    private DocumentSegmentChildChunkService childChunkService;

    @Resource
    private SegmentIndexService segmentIndexService;

    @Resource
    private KbDocumentService kbDocumentService;

    @Resource
    private KnowledgeBaseService knowledgeBaseService;

    @Resource
    private KnowledgeBaseGraphService knowledgeBaseGraphService;

    @Resource
    private IKnowledgeEmbeddingService iKnowledgeEmbeddingService;

    /**
     * 模式感知的分段分页列表：qa 附问题列表，parent_child 附子块列表
     */
    public Page<DocumentSegmentDto> list(String docUuid, int currentPage, int pageSize) {
        KbDocument doc = kbDocumentService.getEnable(docUuid);
        if (doc == null) {
            throw new BaseException(A_DATA_NOT_FOUND);
        }
        Page<DocumentSegment> page = documentSegmentService.lambdaQuery()
                .eq(DocumentSegment::getDocUuid, docUuid)
                .eq(DocumentSegment::getIsDeleted, false)
                .orderByAsc(DocumentSegment::getPosition)
                .page(new Page<>(currentPage, pageSize));
        Page<DocumentSegmentDto> result = new Page<>(page.getCurrent(), page.getSize(), page.getTotal());
        List<DocumentSegmentDto> records = page.getRecords().stream().map(this::toDto).collect(Collectors.toList());

        SegmentModeEnum mode = SegmentIndexService.effectiveMode(doc);
        if (mode == SegmentModeEnum.QA && !records.isEmpty()) {
            Map<Long, List<DocumentSegmentQuestionDto>> byAnswer = questionService.listByDocUuid(docUuid).stream()
                    .map(this::toQuestionDto)
                    .collect(Collectors.groupingBy(DocumentSegmentQuestionDto::getAnswerSegmentId, LinkedHashMap::new, Collectors.toList()));
            records.forEach(dto -> dto.setQuestions(byAnswer.getOrDefault(dto.getId(), List.of())));
        }
        if (mode == SegmentModeEnum.PARENT_CHILD && !records.isEmpty()) {
            Map<Long, List<DocumentSegmentChildChunkDto>> byParent = childChunkService.listByDocUuid(docUuid).stream()
                    .map(this::toChildDto)
                    .collect(Collectors.groupingBy(DocumentSegmentChildChunkDto::getParentSegmentId, LinkedHashMap::new, Collectors.toList()));
            records.forEach(dto -> dto.setChildren(byParent.getOrDefault(dto.getId(), List.of())));
        }
        result.setRecords(records);
        return result;
    }

    /**
     * 编辑主表段内容（text 段文本 / 答案 / 父段）。text 模式会重新向量化该段。
     */
    public boolean editSegment(DocumentSegmentEditReq req) {
        DocumentSegment segment = documentSegmentService.getById(req.getId());
        if (segment == null || Boolean.TRUE.equals(segment.getIsDeleted()) || !segment.getDocUuid().equals(req.getDocUuid())) {
            throw new BaseException(A_DATA_NOT_FOUND);
        }
        KbDocument doc = kbDocumentService.getEnable(segment.getDocUuid());
        if (doc == null) {
            throw new BaseException(A_DATA_NOT_FOUND);
        }
        String oldEmbeddingId = segment.getEmbeddingId();
        segment.setContent(req.getContent());
        documentSegmentService.updateById(segment);

        if (SegmentIndexService.effectiveMode(doc) == SegmentModeEnum.TEXT && oldEmbeddingId != null) {
            KnowledgeBase kb = knowledgeBaseService.getOrThrow(doc.getKbUuid());
            String segmentUuid = segment.getUuid();
            Long segmentId = segment.getId();
            segmentIndexService.reembedSingle(kb, doc, oldEmbeddingId, segmentUuid, req.getContent(),
                    embeddingId -> documentSegmentService.updateEmbeddingId(segmentId, embeddingId));
        }
        return true;
    }

    /**
     * 新增/编辑 QA 问题：
     * id 非空→编辑问题（重嵌）；answerSegmentId 非空→挂到已有答案；否则用 answerContent 新建答案行。
     */
    public DocumentSegmentQuestion saveOrUpdateQuestion(KbDocument doc, KnowledgeBase kb, DocumentSegmentQuestionEditReq req) {
        if (req.getId() != null) {
            DocumentSegmentQuestion question = questionService.getById(req.getId());
            if (question == null || Boolean.TRUE.equals(question.getIsDeleted()) || !question.getDocUuid().equals(req.getDocUuid())) {
                throw new BaseException(A_DATA_NOT_FOUND);
            }
            String oldEmbeddingId = question.getEmbeddingId();
            question.setContent(req.getContent());
            questionService.updateById(question);
            if (oldEmbeddingId != null) {
                segmentIndexService.reembedSingle(kb, doc, oldEmbeddingId, question.getUuid(), req.getContent(),
                        embeddingId -> questionService.updateEmbeddingId(question.getId(), embeddingId));
            }
            return question;
        }
        Long answerSegmentId = req.getAnswerSegmentId();
        if (answerSegmentId == null) {
            if (req.getAnswerContent() == null || req.getAnswerContent().isBlank()) {
                throw new BaseException(A_PARAMS_ERROR);
            }
            DocumentSegment answer = new DocumentSegment();
            answer.setUuid(UuidUtil.createShort());
            answer.setKbUuid(doc.getKbUuid());
            answer.setDocUuid(doc.getUuid());
            answer.setPosition(nextPosition(doc.getUuid()));
            answer.setContent(req.getAnswerContent());
            answer.setHitCount(0);
            answer.setSource(AdiConstant.SegmentSource.MANUAL);
            documentSegmentService.save(answer);
            answerSegmentId = answer.getId();
        } else {
            DocumentSegment answer = documentSegmentService.getById(answerSegmentId);
            if (answer == null || Boolean.TRUE.equals(answer.getIsDeleted()) || !answer.getDocUuid().equals(req.getDocUuid())) {
                throw new BaseException(A_DATA_NOT_FOUND);
            }
        }
        DocumentSegmentQuestion question = new DocumentSegmentQuestion();
        question.setUuid(UuidUtil.createShort());
        question.setKbUuid(doc.getKbUuid());
        question.setDocUuid(doc.getUuid());
        question.setAnswerSegmentId(answerSegmentId);
        question.setPosition(nextQuestionPosition(answerSegmentId));
        question.setContent(req.getContent());
        question.setHitCount(0);
        questionService.save(question);
        // 挂靠答案段停用时不向量化（embeddingId 留空），否则停用段的问题向量复活
        if (isSegmentEnabled(answerSegmentId)) {
            segmentIndexService.reembedSingle(kb, doc, null, question.getUuid(), req.getContent(),
                    embeddingId -> questionService.updateEmbeddingId(question.getId(), embeddingId));
        }
        return question;
    }

    /**
     * 新增/编辑子块：id 非空→编辑（重嵌）；parentSegmentId 非空→父段下追加。
     */
    public DocumentSegmentChildChunk saveOrUpdateChildChunk(KbDocument doc, KnowledgeBase kb, DocumentSegmentChildChunkEditReq req) {
        if (req.getId() != null) {
            DocumentSegmentChildChunk child = childChunkService.getById(req.getId());
            if (child == null || Boolean.TRUE.equals(child.getIsDeleted()) || !child.getDocUuid().equals(req.getDocUuid())) {
                throw new BaseException(A_DATA_NOT_FOUND);
            }
            String oldEmbeddingId = child.getEmbeddingId();
            child.setContent(req.getContent());
            childChunkService.updateById(child);
            if (oldEmbeddingId != null) {
                segmentIndexService.reembedSingle(kb, doc, oldEmbeddingId, child.getUuid(), req.getContent(),
                        embeddingId -> childChunkService.updateEmbeddingId(child.getId(), embeddingId));
            }
            return child;
        }
        if (req.getParentSegmentId() == null) {
            throw new BaseException(A_PARAMS_ERROR);
        }
        DocumentSegment parent = documentSegmentService.getById(req.getParentSegmentId());
        if (parent == null || Boolean.TRUE.equals(parent.getIsDeleted()) || !parent.getDocUuid().equals(req.getDocUuid())) {
            throw new BaseException(A_DATA_NOT_FOUND);
        }
        DocumentSegmentChildChunk child = new DocumentSegmentChildChunk();
        child.setUuid(UuidUtil.createShort());
        child.setKbUuid(doc.getKbUuid());
        child.setDocUuid(doc.getUuid());
        child.setParentSegmentId(parent.getId());
        child.setPosition(childChunkService.listByParentIds(List.of(parent.getId())).size());
        child.setContent(req.getContent());
        child.setHitCount(0);
        childChunkService.save(child);
        // 挂靠父段停用时不向量化（embeddingId 留空），否则停用段的子块向量复活
        if (isSegmentEnabled(parent.getId())) {
            segmentIndexService.reembedSingle(kb, doc, null, child.getUuid(), req.getContent(),
                    embeddingId -> childChunkService.updateEmbeddingId(child.getId(), embeddingId));
        }
        return child;
    }

    /**
     * 删除主表段：text 模式删该段向量；qa 模式级联删其问题；parent_child 级联删其子块
     */
    public boolean deleteSegment(String uuid) {
        DocumentSegment segment = documentSegmentService.lambdaQuery()
                .eq(DocumentSegment::getUuid, uuid)
                .eq(DocumentSegment::getIsDeleted, false)
                .one();
        if (segment == null) {
            throw new BaseException(A_DATA_NOT_FOUND);
        }
        List<String> embeddingIds = new ArrayList<>();
        if (segment.getEmbeddingId() != null) {
            embeddingIds.add(segment.getEmbeddingId());
        }
        List<DocumentSegmentQuestion> questions = questionService.lambdaQuery()
                .eq(DocumentSegmentQuestion::getAnswerSegmentId, segment.getId())
                .eq(DocumentSegmentQuestion::getIsDeleted, false)
                .list();
        questions.forEach(q -> {
            if (q.getEmbeddingId() != null) {
                embeddingIds.add(q.getEmbeddingId());
            }
        });
        List<DocumentSegmentChildChunk> children = childChunkService.listByParentIds(List.of(segment.getId()));
        children.forEach(c -> {
            if (c.getEmbeddingId() != null) {
                embeddingIds.add(c.getEmbeddingId());
            }
        });

        questionService.lambdaUpdate()
                .eq(DocumentSegmentQuestion::getAnswerSegmentId, segment.getId())
                .set(DocumentSegmentQuestion::getIsDeleted, true)
                .update();
        childChunkService.lambdaUpdate()
                .eq(DocumentSegmentChildChunk::getParentSegmentId, segment.getId())
                .set(DocumentSegmentChildChunk::getIsDeleted, true)
                .update();
        documentSegmentService.lambdaUpdate()
                .eq(DocumentSegment::getId, segment.getId())
                .set(DocumentSegment::getIsDeleted, true)
                .update();
        if (!embeddingIds.isEmpty()) {
            iKnowledgeEmbeddingService.deleteByIds(embeddingIds);
        }
        return true;
    }

    /**
     * 删除单个问题（答案保留）
     */
    public boolean deleteQuestion(String uuid) {
        DocumentSegmentQuestion question = questionService.lambdaQuery()
                .eq(DocumentSegmentQuestion::getUuid, uuid)
                .eq(DocumentSegmentQuestion::getIsDeleted, false)
                .one();
        if (question == null) {
            throw new BaseException(A_DATA_NOT_FOUND);
        }
        if (question.getEmbeddingId() != null) {
            iKnowledgeEmbeddingService.deleteByIds(List.of(question.getEmbeddingId()));
        }
        return questionService.lambdaUpdate()
                .eq(DocumentSegmentQuestion::getId, question.getId())
                .set(DocumentSegmentQuestion::getIsDeleted, true)
                .update();
    }

    /**
     * 删除单个子块（父段保留）
     */
    public boolean deleteChildChunk(String uuid) {
        DocumentSegmentChildChunk child = childChunkService.lambdaQuery()
                .eq(DocumentSegmentChildChunk::getUuid, uuid)
                .eq(DocumentSegmentChildChunk::getIsDeleted, false)
                .one();
        if (child == null) {
            throw new BaseException(A_DATA_NOT_FOUND);
        }
        if (child.getEmbeddingId() != null) {
            iKnowledgeEmbeddingService.deleteByIds(List.of(child.getEmbeddingId()));
        }
        return childChunkService.lambdaUpdate()
                .eq(DocumentSegmentChildChunk::getId, child.getId())
                .set(DocumentSegmentChildChunk::getIsDeleted, true)
                .update();
    }

    /**
     * 分段启停（文档级 DOING 中拒绝，避免与索引重跑互相覆盖）。
     * 停用：先删该段名下全部向量 + 账本驱动的图谱足迹清理，最后置位——中途失败即报错、状态不变，
     * 清理操作幂等可直接重试；启用：同步重嵌向量（失败抛异常、状态保持停用），成功后置位并异步重抽图谱。
     */
    public boolean toggleStatus(String uuid, boolean isEnabled) {
        DocumentSegment segment = getDocumentByUuid(uuid);
        KbDocument doc = kbDocumentService.getEnable(segment.getDocUuid());
        if (doc == null) {
            throw new BaseException(A_DATA_NOT_FOUND);
        }
        if (doc.getEmbeddingStatus() == EmbeddingStatusEnum.DOING || doc.getGraphicalStatus() == GraphicalStatusEnum.DOING) {
            throw new BaseException(A_DOC_INDEX_DOING);
        }
        KnowledgeBase kb = knowledgeBaseService.getOrThrow(doc.getKbUuid());
        if (isEnabled) {
            enable(ThreadContext.getCurrentUser(), kb, doc, segment);
        } else {
            disable(kb, doc, segment);
        }
        return true;
    }

    private void disable(KnowledgeBase kb, KbDocument doc, DocumentSegment segment) {
        // 收集该段名下全部向量条目（text=本段；qa=答案下全部问题；parent_child=父段下全部子块）
        List<String> embeddingIds = new ArrayList<>();
        if (segment.getEmbeddingId() != null) {
            embeddingIds.add(segment.getEmbeddingId());
        }
        questionService.listByAnswerIds(List.of(segment.getId())).forEach(q -> {
            if (q.getEmbeddingId() != null) {
                embeddingIds.add(q.getEmbeddingId());
            }
        });
        childChunkService.listByParentIds(List.of(segment.getId())).forEach(c -> {
            if (c.getEmbeddingId() != null) {
                embeddingIds.add(c.getEmbeddingId());
            }
        });
        if (!embeddingIds.isEmpty()) {
            iKnowledgeEmbeddingService.deleteByIds(embeddingIds);
        }
        // 置空三表 embedding_id：统计口径与"embeddingId==null 即待嵌"的既有判断自然正确
        documentSegmentService.clearEmbeddingIdsBySegmentId(segment.getId());
        // 图谱足迹清理（账本驱动：独占元素删除、共享元素保留；幂等）
        knowledgeBaseGraphService.removeSegmentGraphFootprint(kb.getUuid(), segment.getUuid());
        updateStatus(segment.getId(), false);
    }

    private void enable(User user, KnowledgeBase kb, KbDocument doc, DocumentSegment segment) {
        // 同步重嵌（失败抛异常 → 状态保持停用）
        segmentIndexService.vectorizeSegment(kb, doc, segment);
        updateStatus(segment.getId(), true);
        // 图谱异步重建：仅该文档图谱化过（graphicalStatus=DONE）；从未图谱化的文档启用段不触发抽取
        if (doc.getGraphicalStatus() == GraphicalStatusEnum.DONE) {
            kbDocumentService.asyncReGraphSegment(user, kb, doc, segment);
        }
    }

    private void updateStatus(Long segmentId, boolean enabled) {
        documentSegmentService.lambdaUpdate()
                .eq(DocumentSegment::getId, segmentId)
                .set(DocumentSegment::getIsEnabled, enabled)
                .set(DocumentSegment::getEnabledChangeTime, LocalDateTime.now())
                .update();
    }

    private boolean isSegmentEnabled(Long segmentId) {
        DocumentSegment segment = documentSegmentService.getById(segmentId);
        // 历史行 is_enabled 为 null 视为启用
        return segment == null || !Boolean.FALSE.equals(segment.getIsEnabled());
    }

    /**
     * 按uuid取未删除的主表段（删除入口做权限校验用）
     */
    public DocumentSegment getDocumentByUuid(String uuid) {
        DocumentSegment segment = documentSegmentService.lambdaQuery()
                .eq(DocumentSegment::getUuid, uuid)
                .eq(DocumentSegment::getIsDeleted, false)
                .one();
        if (segment == null) {
            throw new BaseException(A_DATA_NOT_FOUND);
        }
        return segment;
    }

    /**
     * 按uuid取未删除的问题行
     */
    public DocumentSegmentQuestion getQuestionByUuid(String uuid) {
        DocumentSegmentQuestion question = questionService.lambdaQuery()
                .eq(DocumentSegmentQuestion::getUuid, uuid)
                .eq(DocumentSegmentQuestion::getIsDeleted, false)
                .one();
        if (question == null) {
            throw new BaseException(A_DATA_NOT_FOUND);
        }
        return question;
    }

    /**
     * 按uuid取未删除的子块行
     */
    public DocumentSegmentChildChunk getChildByUuid(String uuid) {
        DocumentSegmentChildChunk child = childChunkService.lambdaQuery()
                .eq(DocumentSegmentChildChunk::getUuid, uuid)
                .eq(DocumentSegmentChildChunk::getIsDeleted, false)
                .one();
        if (child == null) {
            throw new BaseException(A_DATA_NOT_FOUND);
        }
        return child;
    }

    private int nextPosition(String docUuid) {
        List<DocumentSegment> all = documentSegmentService.listByDocUuid(docUuid);
        return all.isEmpty() ? 0 : all.get(all.size() - 1).getPosition() + 1;
    }

    private int nextQuestionPosition(Long answerSegmentId) {
        List<DocumentSegmentQuestion> questions = questionService.listByAnswerIds(List.of(answerSegmentId));
        return questions.size();
    }

    private DocumentSegmentDto toDto(DocumentSegment segment) {
        DocumentSegmentDto dto = new DocumentSegmentDto();
        BeanUtils.copyProperties(segment, dto);
        return dto;
    }

    private DocumentSegmentQuestionDto toQuestionDto(DocumentSegmentQuestion question) {
        DocumentSegmentQuestionDto dto = new DocumentSegmentQuestionDto();
        BeanUtils.copyProperties(question, dto);
        return dto;
    }

    private DocumentSegmentChildChunkDto toChildDto(DocumentSegmentChildChunk child) {
        DocumentSegmentChildChunkDto dto = new DocumentSegmentChildChunkDto();
        BeanUtils.copyProperties(child, dto);
        return dto;
    }
}

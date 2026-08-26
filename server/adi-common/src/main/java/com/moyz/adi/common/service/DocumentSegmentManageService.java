package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.DocumentSegmentChildChunkDto;
import com.moyz.adi.common.dto.DocumentSegmentChildChunkEditReq;
import com.moyz.adi.common.dto.DocumentSegmentDto;
import com.moyz.adi.common.dto.DocumentSegmentEditReq;
import com.moyz.adi.common.dto.DocumentSegmentQuestionDto;
import com.moyz.adi.common.dto.DocumentSegmentQuestionEditReq;
import com.moyz.adi.common.dto.QaPairEditReq;
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
import com.moyz.adi.common.rag.EmbeddingPresenceChecker;
import com.moyz.adi.common.service.embedding.IKnowledgeEmbeddingService;
import com.moyz.adi.common.util.AdiStringUtil;
import com.moyz.adi.common.util.UuidUtil;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import com.moyz.adi.common.cosntant.AdiConstant;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
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
    private IndexTaskService indexTaskService;

    @Resource
    private IKnowledgeEmbeddingService iKnowledgeEmbeddingService;

    // Optional: only some vector stores support presence checks; absent -> no drift flag
    @Resource
    private ObjectProvider<EmbeddingPresenceChecker> presenceCheckerProvider;

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
        Map<Long, List<DocumentSegmentQuestion>> questionsByAnswer = Map.of();
        Map<Long, List<DocumentSegmentChildChunk>> childrenByParent = Map.of();
        if (mode == SegmentModeEnum.QA && !records.isEmpty()) {
            questionsByAnswer = questionService.listByDocUuid(docUuid).stream()
                    .collect(Collectors.groupingBy(DocumentSegmentQuestion::getAnswerSegmentId, LinkedHashMap::new, Collectors.toList()));
            Map<Long, List<DocumentSegmentQuestionDto>> byAnswer = questionsByAnswer.entrySet().stream()
                    .collect(Collectors.toMap(Map.Entry::getKey,
                            e -> e.getValue().stream().map(this::toQuestionDto).toList(),
                            (a, b) -> a, LinkedHashMap::new));
            records.forEach(dto -> dto.setQuestions(byAnswer.getOrDefault(dto.getId(), List.of())));
        }
        if (mode == SegmentModeEnum.PARENT_CHILD && !records.isEmpty()) {
            childrenByParent = childChunkService.listByDocUuid(docUuid).stream()
                    .collect(Collectors.groupingBy(DocumentSegmentChildChunk::getParentSegmentId, LinkedHashMap::new, Collectors.toList()));
            Map<Long, List<DocumentSegmentChildChunkDto>> byParent = childrenByParent.entrySet().stream()
                    .collect(Collectors.toMap(Map.Entry::getKey,
                            e -> e.getValue().stream().map(this::toChildDto).toList(),
                            (a, b) -> a, LinkedHashMap::new));
            records.forEach(dto -> dto.setChildren(byParent.getOrDefault(dto.getId(), List.of())));
        }
        EmbeddingPresenceChecker checker = presenceCheckerProvider.getIfAvailable();
        if (checker != null && !records.isEmpty()) {
            markVectorMissing(mode, page.getRecords(), records, questionsByAnswer, childrenByParent, checker);
        }
        result.setRecords(records);
        return result;
    }

    /**
     * Drift detection: a backfilled embedding id that the store no longer has means the
     * status column lies. Mode-aware — the vectorized units are the segment row itself (text),
     * its questions (qa) or its child chunks (parent_child). Disabled segments are skipped
     * (their vectors are intentionally absent).
     */
    private void markVectorMissing(SegmentModeEnum mode, List<DocumentSegment> segments, List<DocumentSegmentDto> records,
                                   Map<Long, List<DocumentSegmentQuestion>> questionsByAnswer,
                                   Map<Long, List<DocumentSegmentChildChunk>> childrenByParent,
                                   EmbeddingPresenceChecker checker) {
        Map<Long, DocumentSegmentDto> dtoById = records.stream()
                .collect(Collectors.toMap(DocumentSegmentDto::getId, d -> d));
        Map<Long, List<String>> idsToCheckBySegment = new LinkedHashMap<>();
        for (DocumentSegment segment : segments) {
            if (Boolean.FALSE.equals(segment.getIsEnabled())) {
                continue;
            }
            List<String> ids = switch (mode) {
                case TEXT -> segment.getEmbeddingId() != null ? List.of(segment.getEmbeddingId()) : List.of();
                case QA -> questionsByAnswer.getOrDefault(segment.getId(), List.of()).stream()
                        .map(DocumentSegmentQuestion::getEmbeddingId).filter(Objects::nonNull).toList();
                case PARENT_CHILD -> childrenByParent.getOrDefault(segment.getId(), List.of()).stream()
                        .map(DocumentSegmentChildChunk::getEmbeddingId).filter(Objects::nonNull).toList();
            };
            if (!ids.isEmpty()) {
                idsToCheckBySegment.put(segment.getId(), ids);
            }
        }
        if (idsToCheckBySegment.isEmpty()) {
            return;
        }
        Set<String> existing = checker.findExisting(idsToCheckBySegment.values().stream()
                .flatMap(List::stream).collect(Collectors.toSet()));
        idsToCheckBySegment.forEach((segmentId, ids) -> {
            if (ids.stream().anyMatch(id -> !existing.contains(id))) {
                dtoById.get(segmentId).setVectorMissing(true);
            }
        });
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

        // 一切索引写入走任务队列：text 模式删旧向量+置空后入队重建；qa/parent_child 主行编辑
        // 仅推进段版本（使在途段级图谱任务诚实过期），向量由各自问题/子块编辑路径处理
        bumpSegmentVersion(segment.getId());
        if (SegmentIndexService.effectiveMode(doc) == SegmentModeEnum.TEXT && oldEmbeddingId != null) {
            KnowledgeBase kb = knowledgeBaseService.getOrThrow(doc.getKbUuid());
            iKnowledgeEmbeddingService.deleteByIds(List.of(oldEmbeddingId));
            documentSegmentService.updateEmbeddingId(segment.getId(), null);
            indexTaskService.enqueueSegment(kb, doc, segment, AdiConstant.DOC_INDEX_TYPE_EMBEDDING, ThreadContext.getCurrentUser());
        }
        return true;
    }

    /**
     * Create or edit a QA question:
     * non-null id edits that question (re-embedded); non-null answerSegmentId attaches it to an
     * existing answer; otherwise a new answer row is created from answerContent. One request
     * carries one question; the content is normalized (newlines collapse to spaces, never split)
     * and deduplicated against existing question texts before enqueueing one rebuild.
     */
    public DocumentSegmentQuestion saveOrUpdateQuestion(KbDocument doc, KnowledgeBase kb, DocumentSegmentQuestionEditReq req) {
        String content = AdiStringUtil.normalizeSingleLine(req.getContent());
        if (content.isEmpty()) {
            throw new BaseException(A_PARAMS_ERROR);
        }
        if (req.getId() != null) {
            DocumentSegmentQuestion question = questionService.getById(req.getId());
            if (question == null || Boolean.TRUE.equals(question.getIsDeleted()) || !question.getDocUuid().equals(req.getDocUuid())) {
                throw new BaseException(A_DATA_NOT_FOUND);
            }
            String oldEmbeddingId = question.getEmbeddingId();
            question.setContent(content);
            questionService.updateById(question);
            if (oldEmbeddingId != null) {
                iKnowledgeEmbeddingService.deleteByIds(List.of(oldEmbeddingId));
                questionService.updateEmbeddingId(question.getId(), null);
                enqueueSegmentEmbedding(kb, doc, question.getAnswerSegmentId());
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
        // One question per request (normalized above); skip if the text already exists
        Set<String> existingQuestionTexts = questionService.listByAnswerIds(List.of(answerSegmentId)).stream()
                .map(DocumentSegmentQuestion::getContent)
                .collect(Collectors.toSet());
        if (existingQuestionTexts.contains(content)) {
            throw new BaseException(A_PARAMS_ERROR);
        }
        int position = nextQuestionPosition(answerSegmentId);
        DocumentSegmentQuestion first = null;
        {
            DocumentSegmentQuestion question = new DocumentSegmentQuestion();
            question.setUuid(UuidUtil.createShort());
            question.setKbUuid(doc.getKbUuid());
            question.setDocUuid(doc.getUuid());
            question.setAnswerSegmentId(answerSegmentId);
            question.setPosition(position++);
            question.setContent(content);
            question.setHitCount(0);
            questionService.save(question);
            first = question;
        }
        // 入队重建（执行器跳过停用段；停用段的问题 embeddingId 留空，启用时统一重建）
        enqueueSegmentEmbedding(kb, doc, answerSegmentId);
        return first;
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
                iKnowledgeEmbeddingService.deleteByIds(List.of(oldEmbeddingId));
                childChunkService.updateEmbeddingId(child.getId(), null);
                enqueueSegmentEmbedding(kb, doc, child.getParentSegmentId());
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
        // 入队重建（执行器跳过停用段；停用父段的子块 embeddingId 留空，启用时统一重建）
        enqueueSegmentEmbedding(kb, doc, parent.getId());
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
     * Edit a QA pair: update the answer and replace the question set by content diff.
     * Questions matching the new list are kept as-is (vectors and hit counts preserved);
     * removed ones are deleted with their vectors; new ones are inserted and enqueued for one
     * rebuild. An answer-only change does not re-embed (answers are not vectorized in qa mode).
     * Existing texts are compared after normalization, so legacy multi-line rows are repaired
     * to the normalized text on save.
     */
    @Transactional
    public boolean editQaPair(KbDocument doc, KnowledgeBase kb, QaPairEditReq req) {
        DocumentSegment answer = documentSegmentService.getById(req.getAnswerSegmentId());
        if (answer == null || Boolean.TRUE.equals(answer.getIsDeleted()) || !answer.getDocUuid().equals(req.getDocUuid())) {
            throw new BaseException(A_DATA_NOT_FOUND);
        }
        if (StringUtils.isNotBlank(req.getAnswerContent()) && !req.getAnswerContent().equals(answer.getContent())) {
            answer.setContent(req.getAnswerContent());
            documentSegmentService.updateById(answer);
            // Same semantics as editSegment's qa branch: bump the version to expire in-flight
            // segment tasks; no re-embedding
            bumpSegmentVersion(answer.getId());
        }
        List<String> newTexts = req.getQuestions().stream()
                .map(AdiStringUtil::normalizeSingleLine)
                .filter(StringUtils::isNotBlank)
                .distinct()
                .toList();
        if (newTexts.isEmpty()) {
            throw new BaseException(A_PARAMS_ERROR);
        }
        Set<String> newTextSet = new LinkedHashSet<>(newTexts);
        List<DocumentSegmentQuestion> existing = questionService.listByAnswerIds(List.of(answer.getId()));
        boolean questionsChanged = false;
        List<String> embeddingIdsToRemove = new ArrayList<>();
        // Normalized text is the identity key: legacy multi-line rows compare equal to
        // normalized input, avoiding needless delete-and-rebuild
        Map<String, DocumentSegmentQuestion> existingByNormText = new LinkedHashMap<>();
        for (DocumentSegmentQuestion question : existing) {
            String normText = AdiStringUtil.normalizeSingleLine(question.getContent());
            if (!newTextSet.contains(normText)) {
                if (question.getEmbeddingId() != null) {
                    embeddingIdsToRemove.add(question.getEmbeddingId());
                }
                questionService.lambdaUpdate()
                        .eq(DocumentSegmentQuestion::getId, question.getId())
                        .set(DocumentSegmentQuestion::getIsDeleted, true)
                        .update();
                questionsChanged = true;
            }
            else {
                existingByNormText.putIfAbsent(normText, question);
            }
        }
        if (!embeddingIdsToRemove.isEmpty()) {
            iKnowledgeEmbeddingService.deleteByIds(embeddingIdsToRemove);
        }
        int position = nextQuestionPosition(answer.getId());
        for (String text : newTexts) {
            if (existingByNormText.containsKey(text)) {
                // Kept row with non-normalized stored text (legacy newlines/whitespace):
                // rewrite it to the normalized form
                DocumentSegmentQuestion kept = existingByNormText.get(text);
                if (!text.equals(kept.getContent())) {
                    kept.setContent(text);
                    questionService.updateById(kept);
                }
                continue;
            }
            DocumentSegmentQuestion question = new DocumentSegmentQuestion();
            question.setUuid(UuidUtil.createShort());
            question.setKbUuid(doc.getKbUuid());
            question.setDocUuid(doc.getUuid());
            question.setAnswerSegmentId(answer.getId());
            question.setPosition(position++);
            question.setContent(text);
            question.setHitCount(0);
            questionService.save(question);
            questionsChanged = true;
        }
        if (questionsChanged) {
            enqueueSegmentEmbedding(kb, doc, answer.getId());
        }
        return true;
    }

    /**
     * Repair vector drift of one segment: null out the embedding ids that the store no
     * longer has (mode-aware: segment row / questions / child chunks) and enqueue one
     * segment re-embedding task. No-op when nothing is missing or no checker is available.
     */
    public boolean repairSegmentVector(String segmentUuid) {
        EmbeddingPresenceChecker checker = presenceCheckerProvider.getIfAvailable();
        if (checker == null) {
            return false;
        }
        DocumentSegment segment = documentSegmentService.lambdaQuery()
                .eq(DocumentSegment::getUuid, segmentUuid)
                .eq(DocumentSegment::getIsDeleted, false)
                .one();
        if (segment == null) {
            throw new BaseException(A_DATA_NOT_FOUND);
        }
        // Disabled segments intentionally have no vectors
        if (Boolean.FALSE.equals(segment.getIsEnabled())) {
            return false;
        }
        KbDocument doc = kbDocumentService.getEnable(segment.getDocUuid());
        if (doc == null) {
            throw new BaseException(A_DATA_NOT_FOUND);
        }
        KnowledgeBase kb = knowledgeBaseService.getOrThrow(doc.getKbUuid());
        SegmentModeEnum mode = SegmentIndexService.effectiveMode(doc);
        List<String> toCheck = switch (mode) {
            case TEXT -> segment.getEmbeddingId() != null ? List.of(segment.getEmbeddingId()) : List.of();
            case QA -> questionService.listByAnswerIds(List.of(segment.getId())).stream()
                    .map(DocumentSegmentQuestion::getEmbeddingId).filter(Objects::nonNull).toList();
            case PARENT_CHILD -> childChunkService.listByParentIds(List.of(segment.getId())).stream()
                    .map(DocumentSegmentChildChunk::getEmbeddingId).filter(Objects::nonNull).toList();
        };
        if (toCheck.isEmpty()) {
            return false;
        }
        Set<String> existing = checker.findExisting(toCheck);
        boolean changed = false;
        if (mode == SegmentModeEnum.TEXT) {
            if (!existing.contains(segment.getEmbeddingId())) {
                documentSegmentService.updateEmbeddingId(segment.getId(), null);
                changed = true;
            }
        } else if (mode == SegmentModeEnum.QA) {
            for (DocumentSegmentQuestion question : questionService.listByAnswerIds(List.of(segment.getId()))) {
                if (question.getEmbeddingId() != null && !existing.contains(question.getEmbeddingId())) {
                    questionService.updateEmbeddingId(question.getId(), null);
                    changed = true;
                }
            }
        } else {
            for (DocumentSegmentChildChunk child : childChunkService.listByParentIds(List.of(segment.getId()))) {
                if (child.getEmbeddingId() != null && !existing.contains(child.getEmbeddingId())) {
                    childChunkService.updateEmbeddingId(child.getId(), null);
                    changed = true;
                }
            }
        }
        if (changed) {
            indexTaskService.enqueueSegment(kb, doc, segment, AdiConstant.DOC_INDEX_TYPE_EMBEDDING, ThreadContext.getCurrentUser());
        }
        return changed;
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
     * 停用：先删该段名下全部向量 + 账本驱动的图谱足迹清理，最后置位--中途失败即报错、状态不变，
     * 清理操作幂等可直接重试；启用：立即置位并把两路索引重建交给异步任务（段级状态字段标记进度，
     * 重建中/失败可在列表上观测，对已启用但状态 FAIL 的段重复调用即幂等重试）。
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
        // 同 doc 有队列任务在跑时拒绝：disable 的同步清理会与任务写入交错
        if (indexTaskService.hasRunningByDoc(doc.getUuid())) {
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
        // 置位 + 索引数据已清，重建状态归 NONE
        documentSegmentService.lambdaUpdate()
                .eq(DocumentSegment::getId, segment.getId())
                .set(DocumentSegment::getIsEnabled, false)
                .set(DocumentSegment::getEnabledChangeTime, LocalDateTime.now())
                .set(DocumentSegment::getEmbeddingStatus, EmbeddingStatusEnum.NONE)
                .set(DocumentSegment::getGraphicalStatus, GraphicalStatusEnum.NONE)
                .set(DocumentSegment::getFailReason, "")
                .update();
    }

    private void enable(User user, KnowledgeBase kb, KbDocument doc, DocumentSegment segment) {
        // 从未图谱化的文档无需图谱重建，直接标记就绪
        boolean graphRebuildNeeded = doc.getGraphicalStatus() == GraphicalStatusEnum.DONE;
        documentSegmentService.lambdaUpdate()
                .eq(DocumentSegment::getId, segment.getId())
                .set(DocumentSegment::getIsEnabled, true)
                .set(DocumentSegment::getEnabledChangeTime, LocalDateTime.now())
                .set(DocumentSegment::getEmbeddingStatus, EmbeddingStatusEnum.DOING)
                .set(DocumentSegment::getGraphicalStatus, graphRebuildNeeded ? GraphicalStatusEnum.DOING : GraphicalStatusEnum.DONE)
                .set(DocumentSegment::getFailReason, "")
                .update();
        // 重建走任务队列（同 doc 串行，状态字段标记进度，失败可重试）
        indexTaskService.enqueueSegment(kb, doc, segment, AdiConstant.DOC_INDEX_TYPE_EMBEDDING, user);
        if (graphRebuildNeeded) {
            indexTaskService.enqueueSegment(kb, doc, segment, AdiConstant.DOC_INDEX_TYPE_GRAPHICAL, user);
        }
    }

    /**
     * 段索引版本原子推进：段内容相关变更调用（在途段级任务经检查点作废）
     */
    private void bumpSegmentVersion(Long segmentId) {
        documentSegmentService.lambdaUpdate()
                .eq(DocumentSegment::getId, segmentId)
                .setSql("index_version = index_version + 1")
                .update();
    }

    /**
     * 问题/子块编辑后按所属段入队重建（答案段/父段行即段级任务目标）
     */
    private void enqueueSegmentEmbedding(KnowledgeBase kb, KbDocument doc, Long segmentId) {
        DocumentSegment segment = documentSegmentService.getById(segmentId);
        if (segment != null) {
            bumpSegmentVersion(segment.getId());
            indexTaskService.enqueueSegment(kb, doc, segment, AdiConstant.DOC_INDEX_TYPE_EMBEDDING, ThreadContext.getCurrentUser());
        }
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

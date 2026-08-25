package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.dto.RefEmbeddingDto;
import com.moyz.adi.common.entity.DocumentSegment;
import com.moyz.adi.common.entity.DocumentSegmentChildChunk;
import com.moyz.adi.common.entity.DocumentSegmentQuestion;
import com.moyz.adi.common.enums.SegmentModeEnum;
import com.moyz.adi.common.mapper.DocumentSegmentMapper;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * 文档分段关系层服务——分段内容与元数据的唯一事实源。
 * text 模式行自身被向量化；qa 模式行为答案（问题在子表并向量化）；
 * parent_child 模式行为父段（子块在子表并向量化）。
 */
@Slf4j
@Service
public class DocumentSegmentService extends ServiceImpl<DocumentSegmentMapper, DocumentSegment> {

    @Resource
    private DocumentSegmentQuestionService questionService;

    @Resource
    private DocumentSegmentChildChunkService childChunkService;

    /**
     * 按文档uuid列出主表段（未删除，按顺序）
     */
    public List<DocumentSegment> listByDocUuid(String docUuid) {
        return lambdaQuery()
                .eq(DocumentSegment::getDocUuid, docUuid)
                .eq(DocumentSegment::getIsDeleted, false)
                .orderByAsc(DocumentSegment::getPosition)
                .list();
    }

    /**
     * 按向量库条目id批量查主表段行（text 模式命中）
     */
    public List<DocumentSegment> listByEmbeddingIds(List<String> embeddingIds) {
        if (CollectionUtils.isEmpty(embeddingIds)) {
            return Collections.emptyList();
        }
        return lambdaQuery()
                .in(DocumentSegment::getEmbeddingId, embeddingIds)
                .list();
    }

    /**
     * 回填单条主表段行的向量条目id
     */
    public void updateEmbeddingId(Long id, String embeddingId) {
        lambdaUpdate()
                .eq(DocumentSegment::getId, id)
                .set(DocumentSegment::getEmbeddingId, embeddingId)
                .update();
    }

    /**
     * 软删除某文档下的全部段行及子表数据（问题、子块级联）
     */
    public void deleteByDocUuid(String docUuid) {
        lambdaUpdate()
                .eq(DocumentSegment::getDocUuid, docUuid)
                .set(DocumentSegment::getIsDeleted, true)
                .update();
        questionService.deleteByDocUuid(docUuid);
        childChunkService.deleteByDocUuid(docUuid);
    }

    /**
     * 清空该段名下全部向量条目id（主表行 + 问题 + 子块；段停用删向量后调用，
     * 使统计口径与"embeddingId == null 即待嵌"的既有判断自然正确）
     */
    public void clearEmbeddingIdsBySegmentId(Long segmentId) {
        lambdaUpdate()
                .eq(DocumentSegment::getId, segmentId)
                .set(DocumentSegment::getEmbeddingId, null)
                .update();
        questionService.clearEmbeddingIdsByAnswerIds(List.of(segmentId));
        childChunkService.clearEmbeddingIdsByParentIds(List.of(segmentId));
    }

    /**
     * 按文档uuid取启用中的段（重嵌/图谱抽取时过滤停用段用；历史行 is_enabled 为 null 视为启用）
     */
    public List<DocumentSegment> listEnabledByDocUuid(String docUuid) {
        return lambdaQuery()
                .eq(DocumentSegment::getDocUuid, docUuid)
                .eq(DocumentSegment::getIsDeleted, false)
                .and(q -> q.eq(DocumentSegment::getIsEnabled, true).or().isNull(DocumentSegment::getIsEnabled))
                .orderByAsc(DocumentSegment::getPosition)
                .list();
    }

    /**
     * 取启用段id集合（问题/子块重嵌时过滤停用答案/父段用）
     */
    public Set<Long> listEnabledIdsByDocUuid(String docUuid) {
        return listEnabledByDocUuid(docUuid).stream().map(DocumentSegment::getId).collect(Collectors.toSet());
    }

    /**
     * 段级命中统计：按向量条目id分发到三张表累加，并把 qa/parent_child 模式的命中传导 +1 到答案/父段行
     */
    public void incrementHitCounts(List<String> embeddingIds) {
        if (CollectionUtils.isEmpty(embeddingIds)) {
            return;
        }
        baseMapper.incrementHitCountByEmbeddingIds(embeddingIds);
        questionService.getBaseMapper().incrementHitCountByEmbeddingIds(embeddingIds);
        childChunkService.getBaseMapper().incrementHitCountByEmbeddingIds(embeddingIds);

        // 传导：问题命中 → 答案行；子块命中 → 父段行
        Set<Long> propagatedIds = new HashSet<>();
        questionService.listByEmbeddingIds(embeddingIds).forEach(q -> propagatedIds.add(q.getAnswerSegmentId()));
        childChunkService.listByEmbeddingIds(embeddingIds).forEach(c -> propagatedIds.add(c.getParentSegmentId()));
        if (!propagatedIds.isEmpty()) {
            baseMapper.incrementHitCountByIds(new ArrayList<>(propagatedIds));
        }
    }

    /**
     * 按向量条目id反查文档uuid（三表并集），用于文档级命中统计
     */
    public List<String> selectDocUuidsByEmbeddingIds(List<String> embeddingIds) {
        if (CollectionUtils.isEmpty(embeddingIds)) {
            return Collections.emptyList();
        }
        Set<String> docUuids = Stream.of(
                        listByEmbeddingIds(embeddingIds).stream().map(DocumentSegment::getDocUuid),
                        questionService.listByEmbeddingIds(embeddingIds).stream().map(DocumentSegmentQuestion::getDocUuid),
                        childChunkService.listByEmbeddingIds(embeddingIds).stream().map(DocumentSegmentChildChunk::getDocUuid))
                .flatMap(Function.identity())
                .filter(StringUtils::isNotBlank)
                .collect(Collectors.toSet());
        return new ArrayList<>(docUuids);
    }

    /**
     * 统计知识库下被向量化的条目数（text 段 + qa 问题 + 子块），替代旧向量表 count
     */
    public int countVectorizedByKbUuid(String kbUuid) {
        long main = lambdaQuery()
                .eq(DocumentSegment::getKbUuid, kbUuid)
                .eq(DocumentSegment::getIsDeleted, false)
                .isNotNull(DocumentSegment::getEmbeddingId)
                .count();
        long questions = questionService.lambdaQuery()
                .eq(DocumentSegmentQuestion::getKbUuid, kbUuid)
                .eq(DocumentSegmentQuestion::getIsDeleted, false)
                .count();
        long children = childChunkService.lambdaQuery()
                .eq(DocumentSegmentChildChunk::getKbUuid, kbUuid)
                .eq(DocumentSegmentChildChunk::getIsDeleted, false)
                .count();
        return (int) (main + questions + children);
    }

    /**
     * 溯源展示：按向量条目id取命中内容（text→段内容；问题→答案内容；子块→父段内容）。
     * matchedText 记录命中的向量化单元（问题/子块文本），text 是返回给 LLM 的展开内容，
     * segmentMode 告知前端按哪种结构展示
     */
    public List<RefEmbeddingDto> listRefTextsByEmbeddingIds(List<String> embeddingIds) {
        if (CollectionUtils.isEmpty(embeddingIds)) {
            return Collections.emptyList();
        }
        Map<String, DocumentSegment> mainByEmb = mapByEmbeddingIds(embeddingIds);

        List<DocumentSegmentQuestion> questionHits = questionService.listByEmbeddingIds(embeddingIds);
        Map<String, DocumentSegmentQuestion> questionByEmb = questionHits.stream()
                .filter(q -> q.getEmbeddingId() != null)
                .collect(Collectors.toMap(DocumentSegmentQuestion::getEmbeddingId, Function.identity(), (a, b) -> a));

        List<DocumentSegmentChildChunk> childHits = childChunkService.listByEmbeddingIds(embeddingIds);
        Map<String, DocumentSegmentChildChunk> childByEmb = childHits.stream()
                .filter(c -> c.getEmbeddingId() != null)
                .collect(Collectors.toMap(DocumentSegmentChildChunk::getEmbeddingId, Function.identity(), (a, b) -> a));

        // 二跳：答案/父段内容
        Set<Long> secondHopIds = new HashSet<>();
        questionHits.forEach(q -> secondHopIds.add(q.getAnswerSegmentId()));
        childHits.forEach(c -> secondHopIds.add(c.getParentSegmentId()));
        Map<Long, DocumentSegment> mainById = mapByIds(secondHopIds);

        List<RefEmbeddingDto> result = new ArrayList<>();
        for (String embeddingId : embeddingIds) {
            RefEmbeddingDto dto = resolveRefDto(embeddingId, mainByEmb, questionByEmb, childByEmb, mainById);
            if (dto != null) {
                result.add(dto);
            }
        }
        return result;
    }

    private RefEmbeddingDto resolveRefDto(String embeddingId,
                                          Map<String, DocumentSegment> mainByEmb,
                                          Map<String, DocumentSegmentQuestion> questionByEmb,
                                          Map<String, DocumentSegmentChildChunk> childByEmb,
                                          Map<Long, DocumentSegment> mainById) {
        DocumentSegment main = mainByEmb.get(embeddingId);
        if (main != null) {
            return RefEmbeddingDto.builder()
                    .embeddingId(embeddingId)
                    .text(main.getContent())
                    .segmentMode(SegmentModeEnum.TEXT.getValue())
                    .build();
        }
        DocumentSegmentQuestion question = questionByEmb.get(embeddingId);
        if (question != null) {
            DocumentSegment answer = mainById.get(question.getAnswerSegmentId());
            return RefEmbeddingDto.builder()
                    .embeddingId(embeddingId)
                    .matchedText(question.getContent())
                    .text(answer != null ? answer.getContent() : question.getContent())
                    .segmentMode(SegmentModeEnum.QA.getValue())
                    .build();
        }
        DocumentSegmentChildChunk child = childByEmb.get(embeddingId);
        if (child != null) {
            DocumentSegment parent = mainById.get(child.getParentSegmentId());
            return RefEmbeddingDto.builder()
                    .embeddingId(embeddingId)
                    .matchedText(child.getContent())
                    .text(parent != null ? parent.getContent() : child.getContent())
                    .segmentMode(SegmentModeEnum.PARENT_CHILD.getValue())
                    .build();
        }
        return null;
    }

    /**
     * 按向量条目id批量取主表段映射（检索展开用）
     */
    public Map<String, DocumentSegment> mapByEmbeddingIds(List<String> embeddingIds) {
        return listByEmbeddingIds(embeddingIds).stream()
                .filter(s -> s.getEmbeddingId() != null)
                .collect(Collectors.toMap(DocumentSegment::getEmbeddingId, Function.identity(), (a, b) -> a));
    }

    /**
     * 按段id集合批量取主表段映射
     */
    public Map<Long, DocumentSegment> mapByIds(Set<Long> ids) {
        if (CollectionUtils.isEmpty(ids)) {
            return Collections.emptyMap();
        }
        return listByIds(ids).stream()
                .filter(Objects::nonNull)
                .collect(Collectors.toMap(DocumentSegment::getId, Function.identity(), (a, b) -> a));
    }
}

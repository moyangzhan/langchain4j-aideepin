package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.entity.DocumentSegmentQuestion;
import com.moyz.adi.common.mapper.DocumentSegmentQuestionMapper;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.List;

@Service
public class DocumentSegmentQuestionService extends ServiceImpl<DocumentSegmentQuestionMapper, DocumentSegmentQuestion> {

    /**
     * 按文档uuid列出问题（未删除，按答案分段+顺序）
     */
    public List<DocumentSegmentQuestion> listByDocUuid(String docUuid) {
        return lambdaQuery()
                .eq(DocumentSegmentQuestion::getDocUuid, docUuid)
                .eq(DocumentSegmentQuestion::getIsDeleted, false)
                .orderByAsc(DocumentSegmentQuestion::getAnswerSegmentId)
                .orderByAsc(DocumentSegmentQuestion::getPosition)
                .list();
    }

    /**
     * 按向量库条目id批量查问题行
     */
    public List<DocumentSegmentQuestion> listByEmbeddingIds(List<String> embeddingIds) {
        if (CollectionUtils.isEmpty(embeddingIds)) {
            return Collections.emptyList();
        }
        return lambdaQuery()
                .in(DocumentSegmentQuestion::getEmbeddingId, embeddingIds)
                .list();
    }

    /**
     * 按答案分段id批量查问题行
     */
    public List<DocumentSegmentQuestion> listByAnswerIds(List<Long> answerSegmentIds) {
        if (CollectionUtils.isEmpty(answerSegmentIds)) {
            return Collections.emptyList();
        }
        return lambdaQuery()
                .in(DocumentSegmentQuestion::getAnswerSegmentId, answerSegmentIds)
                .list();
    }

    /**
     * 重置某文档下所有问题的向量条目id（重新向量化前调用，清除悬空引用）
     */
    public void clearEmbeddingIds(String docUuid) {
        lambdaUpdate()
                .eq(DocumentSegmentQuestion::getDocUuid, docUuid)
                .set(DocumentSegmentQuestion::getEmbeddingId, null)
                .update();
    }

    /**
     * 清空指定答案段下全部问题的向量条目id（答案段停用删向量后调用）
     */
    public void clearEmbeddingIdsByAnswerIds(List<Long> answerSegmentIds) {
        if (CollectionUtils.isEmpty(answerSegmentIds)) {
            return;
        }
        lambdaUpdate()
                .in(DocumentSegmentQuestion::getAnswerSegmentId, answerSegmentIds)
                .set(DocumentSegmentQuestion::getEmbeddingId, null)
                .update();
    }

    /**
     * 回填单条问题行的向量条目id
     */
    public void updateEmbeddingId(Long id, String embeddingId) {
        lambdaUpdate()
                .eq(DocumentSegmentQuestion::getId, id)
                .set(DocumentSegmentQuestion::getEmbeddingId, embeddingId)
                .update();
    }

    /**
     * 软删除某文档下的全部问题行
     */
    public void deleteByDocUuid(String docUuid) {
        lambdaUpdate()
                .eq(DocumentSegmentQuestion::getDocUuid, docUuid)
                .set(DocumentSegmentQuestion::getIsDeleted, true)
                .update();
    }
}

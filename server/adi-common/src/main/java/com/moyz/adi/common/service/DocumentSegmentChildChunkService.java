package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.entity.DocumentSegmentChildChunk;
import com.moyz.adi.common.mapper.DocumentSegmentChildChunkMapper;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.List;

@Service
public class DocumentSegmentChildChunkService extends ServiceImpl<DocumentSegmentChildChunkMapper, DocumentSegmentChildChunk> {

    /**
     * 按文档uuid列出子块（未删除，按父分段+顺序）
     */
    public List<DocumentSegmentChildChunk> listByDocUuid(String docUuid) {
        return lambdaQuery()
                .eq(DocumentSegmentChildChunk::getDocUuid, docUuid)
                .eq(DocumentSegmentChildChunk::getIsDeleted, false)
                .orderByAsc(DocumentSegmentChildChunk::getParentSegmentId)
                .orderByAsc(DocumentSegmentChildChunk::getPosition)
                .list();
    }

    /**
     * 按向量库条目id批量查子块行
     */
    public List<DocumentSegmentChildChunk> listByEmbeddingIds(List<String> embeddingIds) {
        if (CollectionUtils.isEmpty(embeddingIds)) {
            return Collections.emptyList();
        }
        return lambdaQuery()
                .in(DocumentSegmentChildChunk::getEmbeddingId, embeddingIds)
                .list();
    }

    /**
     * 按父分段id批量查子块行
     */
    public List<DocumentSegmentChildChunk> listByParentIds(List<Long> parentSegmentIds) {
        if (CollectionUtils.isEmpty(parentSegmentIds)) {
            return Collections.emptyList();
        }
        return lambdaQuery()
                .in(DocumentSegmentChildChunk::getParentSegmentId, parentSegmentIds)
                .list();
    }

    /**
     * 重置某文档下所有子块的向量条目id（重新向量化前调用，清除悬空引用）
     */
    public void clearEmbeddingIds(String docUuid) {
        lambdaUpdate()
                .eq(DocumentSegmentChildChunk::getDocUuid, docUuid)
                .set(DocumentSegmentChildChunk::getEmbeddingId, null)
                .update();
    }

    /**
     * 清空指定父段下全部子块的向量条目id（父段停用删向量后调用）
     */
    public void clearEmbeddingIdsByParentIds(List<Long> parentSegmentIds) {
        if (CollectionUtils.isEmpty(parentSegmentIds)) {
            return;
        }
        lambdaUpdate()
                .in(DocumentSegmentChildChunk::getParentSegmentId, parentSegmentIds)
                .set(DocumentSegmentChildChunk::getEmbeddingId, null)
                .update();
    }

    /**
     * 回填单条子块行的向量条目id
     */
    public void updateEmbeddingId(Long id, String embeddingId) {
        lambdaUpdate()
                .eq(DocumentSegmentChildChunk::getId, id)
                .set(DocumentSegmentChildChunk::getEmbeddingId, embeddingId)
                .update();
    }

    /**
     * 软删除某文档下的全部子块行
     */
    public void deleteByDocUuid(String docUuid) {
        lambdaUpdate()
                .eq(DocumentSegmentChildChunk::getDocUuid, docUuid)
                .set(DocumentSegmentChildChunk::getIsDeleted, true)
                .update();
    }
}

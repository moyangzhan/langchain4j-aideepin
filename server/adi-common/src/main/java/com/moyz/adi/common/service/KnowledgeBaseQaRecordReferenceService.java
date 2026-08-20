package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.dto.RefEmbeddingDto;
import com.moyz.adi.common.entity.KnowledgeBaseQaRefEmbedding;
import com.moyz.adi.common.mapper.KnowledgeBaseQaRecordReferenceMapper;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.List;

@Slf4j
@Service
public class KnowledgeBaseQaRecordReferenceService extends ServiceImpl<KnowledgeBaseQaRecordReferenceMapper, KnowledgeBaseQaRefEmbedding> {

    /**
     * 段内容唯一事实源在关系表：text→段内容；问题→答案内容；子块→父段内容
     */
    @Resource
    private DocumentSegmentService documentSegmentService;

    public List<RefEmbeddingDto> listRefEmbeddings(String aqRecordUuid) {
        List<KnowledgeBaseQaRefEmbedding> recordReferences = this.getBaseMapper().listByQaUuid(aqRecordUuid);
        if (CollectionUtils.isEmpty(recordReferences)) {
            return Collections.emptyList();
        }
        List<String> embeddingIds = recordReferences.stream().map(KnowledgeBaseQaRefEmbedding::getEmbeddingId).toList();
        if (CollectionUtils.isEmpty(embeddingIds)) {
            return Collections.emptyList();
        }
        return documentSegmentService.listRefTextsByEmbeddingIds(embeddingIds);
    }
}

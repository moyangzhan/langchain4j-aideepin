package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.dto.RefEmbeddingDto;
import com.moyz.adi.common.entity.KnowledgeBaseAskRefEmbedding;
import com.moyz.adi.common.mapper.KnowledgeBaseAskRefEmbeddingMapper;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.List;

@Slf4j
@Service
public class KnowledgeBaseAskRefEmbeddingService extends ServiceImpl<KnowledgeBaseAskRefEmbeddingMapper, KnowledgeBaseAskRefEmbedding> {

    /**
     * 段内容唯一事实源在关系表：text→段内容；问题→答案内容；子块→父段内容
     */
    @Resource
    private DocumentSegmentService documentSegmentService;

    public List<RefEmbeddingDto> listRefEmbeddings(String askRecordUuid) {
        List<KnowledgeBaseAskRefEmbedding> recordReferences = this.getBaseMapper().listByAskUuid(askRecordUuid);
        if (CollectionUtils.isEmpty(recordReferences)) {
            return Collections.emptyList();
        }
        List<String> embeddingIds = recordReferences.stream().map(KnowledgeBaseAskRefEmbedding::getEmbeddingId).toList();
        if (CollectionUtils.isEmpty(embeddingIds)) {
            return Collections.emptyList();
        }
        return documentSegmentService.listRefTextsByEmbeddingIds(embeddingIds);
    }
}

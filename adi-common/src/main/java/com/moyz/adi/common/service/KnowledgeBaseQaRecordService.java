package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.baomidou.mybatisplus.extension.toolkit.ChainWrappers;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.KbItemEmbeddingDto;
import com.moyz.adi.common.dto.KbQaRecordDto;
import com.moyz.adi.common.dto.KbQaRecordReferenceDto;
import com.moyz.adi.common.entity.*;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.mapper.KnowledgeBaseQaRecordMapper;
import com.moyz.adi.common.util.MPPageUtil;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

import java.util.*;

import static com.moyz.adi.common.enums.ErrorEnum.A_DATA_NOT_FOUND;
import static com.moyz.adi.common.util.LocalCache.MODEL_ID_TO_OBJ;

@Slf4j
@Service
public class KnowledgeBaseQaRecordService extends ServiceImpl<KnowledgeBaseQaRecordMapper, KnowledgeBaseQaRecord> {

    @Resource
    private KnowledgeBaseQaRecordReferenceService knowledgeBaseQaRecordReferenceService;

    @Resource
    private KnowledgeBaseEmbeddingService knowledgeBaseEmbeddingService;

    public Page<KbQaRecordDto> search(String kbUuid, String keyword, Integer currentPage, Integer pageSize) {
        LambdaQueryWrapper<KnowledgeBaseQaRecord> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(KnowledgeBaseQaRecord::getKbUuid, kbUuid);
        wrapper.eq(KnowledgeBaseQaRecord::getIsDeleted, false);
        if (Boolean.FALSE.equals(ThreadContext.getCurrentUser().getIsAdmin())) {
            wrapper.eq(KnowledgeBaseQaRecord::getUserId, ThreadContext.getCurrentUserId());
        }
        if (StringUtils.isNotBlank(keyword)) {
            wrapper.like(KnowledgeBaseQaRecord::getQuestion, keyword);
        }
        wrapper.orderByDesc(KnowledgeBaseQaRecord::getUpdateTime);
        Page<KnowledgeBaseQaRecord> page = baseMapper.selectPage(new Page<>(currentPage, pageSize), wrapper);

        Page<KbQaRecordDto> result = new Page<>();
        MPPageUtil.convertToPage(page, result, KbQaRecordDto.class, (t1, t2) -> {
            AiModel aiModel = MODEL_ID_TO_OBJ.get(t1.getAiModelId());
            t2.setAiModelPlatform(null == aiModel ? "" : aiModel.getPlatform());
            return t2;
        });
        return result;
    }

    /**
     * 创建新的QA及引用记录
     *
     * @param user
     * @param knowledgeBase    所属的知识库
     * @param newRecord
     * @param embeddingToScore
     */
    public void createRecordAndReferences(User user, KnowledgeBase knowledgeBase, KnowledgeBaseQaRecord newRecord, Map<String, Double> embeddingToScore) {
        String uuid = UUID.randomUUID().toString().replace("-", "");
        newRecord.setKbId(knowledgeBase.getId());
        newRecord.setKbUuid((knowledgeBase.getUuid()));
        newRecord.setUuid(uuid);
        newRecord.setUserId(user.getId());
        baseMapper.insert(newRecord);

        for (String embeddingId : embeddingToScore.keySet()) {
            KnowledgeBaseQaRecordReference recordReference = new KnowledgeBaseQaRecordReference();
            recordReference.setQaRecordId(newRecord.getId());
            recordReference.setEmbeddingId(embeddingId);
            recordReference.setScore(embeddingToScore.get(embeddingId));
            recordReference.setUserId(user.getId());
            knowledgeBaseQaRecordReferenceService.save(recordReference);
        }
    }

    public List<KbQaRecordReferenceDto> listReferences(String uuid) {
        List<KnowledgeBaseQaRecordReference> recordReferences = knowledgeBaseQaRecordReferenceService.listByQaUuid(uuid);
        if (CollectionUtils.isEmpty(recordReferences)) {
            return Collections.emptyList();
        }
        List<String> embeddingIds = recordReferences.stream().map(KnowledgeBaseQaRecordReference::getEmbeddingId).toList();
        if (CollectionUtils.isEmpty(embeddingIds)) {
            return Collections.emptyList();
        }
        List<KnowledgeBaseEmbedding> embeddings = knowledgeBaseEmbeddingService.listByEmbeddingIds(embeddingIds);
        List<KbQaRecordReferenceDto> result = new ArrayList<>();
        for (KnowledgeBaseEmbedding embedding : embeddings) {
            KbQaRecordReferenceDto newOne = KbQaRecordReferenceDto.builder()
                    .embeddingId(embedding.getEmbeddingId().toString())
                    .text(embedding.getText())
                    .build();
            result.add(newOne);
        }
        return result;
    }

    public boolean softDelete(String uuid) {
        if (Boolean.TRUE.equals(ThreadContext.getCurrentUser().getIsAdmin())) {
            return ChainWrappers.lambdaUpdateChain(baseMapper)
                    .eq(KnowledgeBaseQaRecord::getUuid, uuid)
                    .set(KnowledgeBaseQaRecord::getIsDeleted, true)
                    .update();
        }
        KnowledgeBaseQaRecord exist = ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(KnowledgeBaseQaRecord::getUuid, uuid)
                .one();
        if (null == exist) {
            throw new BaseException(A_DATA_NOT_FOUND);
        }
        return ChainWrappers.lambdaUpdateChain(baseMapper)
                .eq(KnowledgeBaseQaRecord::getId, exist.getId())
                .set(KnowledgeBaseQaRecord::getIsDeleted, true)
                .update();
    }
}

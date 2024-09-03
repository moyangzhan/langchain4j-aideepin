package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.baomidou.mybatisplus.extension.toolkit.ChainWrappers;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.*;
import com.moyz.adi.common.entity.*;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.mapper.KnowledgeBaseQaRecordMapper;
import com.moyz.adi.common.util.MPPageUtil;
import com.moyz.adi.common.util.UuidUtil;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
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

    @Resource
    private AiModelService aiModelService;

    public KbQaRecordDto add(KnowledgeBase knowledgeBase, QARecordReq req) {
        KnowledgeBaseQaRecord newRecord = new KnowledgeBaseQaRecord();
        newRecord.setAiModelId(aiModelService.getIdByName(req.getModelName()));
        newRecord.setQuestion(req.getQuestion());
        newRecord.setKbId(knowledgeBase.getId());
        newRecord.setKbUuid((knowledgeBase.getUuid()));
        newRecord.setUuid(UuidUtil.createShort());
        newRecord.setUserId(ThreadContext.getCurrentUserId());
        baseMapper.insert(newRecord);

        KbQaRecordDto result = new KbQaRecordDto();
        BeanUtils.copyProperties(newRecord, result);
        return result;
    }

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
     * 增加引用记录
     *
     * @param user
     * @param qaRecordId       qa记录id
     * @param embeddingToScore
     */
    public void createReferences(User user, Long qaRecordId, Map<String, Double> embeddingToScore) {
        for (String embeddingId : embeddingToScore.keySet()) {
            KnowledgeBaseQaRecordReference recordReference = new KnowledgeBaseQaRecordReference();
            recordReference.setQaRecordId(qaRecordId);
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

    public KnowledgeBaseQaRecord getOrThrow(String uuid) {
        KnowledgeBaseQaRecord exist = ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(KnowledgeBaseQaRecord::getUuid, uuid)
                .eq(KnowledgeBaseQaRecord::getIsDeleted, false)
                .one();
        if (null == exist) {
            throw new BaseException(A_DATA_NOT_FOUND);
        }
        return exist;
    }

    public void clearByCurrentUser() {
        ChainWrappers.lambdaUpdateChain(baseMapper)
                .eq(KnowledgeBaseQaRecord::getUserId, ThreadContext.getCurrentUserId())
                .set(KnowledgeBaseQaRecord::getIsDeleted, true)
                .update();
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

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
import com.moyz.adi.common.service.embedding.IEmbeddingService;
import com.moyz.adi.common.util.JsonUtil;
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
public class KnowledgeBaseQaService extends ServiceImpl<KnowledgeBaseQaRecordMapper, KnowledgeBaseQa> {

    @Resource
    private KnowledgeBaseQaRecordReferenceService knowledgeBaseQaRecordReferenceService;

    @Resource
    private KnowledgeBaseQaRefGraphService knowledgeBaseQaRecordRefGraphService;

    @Resource
    private IEmbeddingService iEmbeddingService;

    @Resource
    private AiModelService aiModelService;

    public KbQaDto add(KnowledgeBase knowledgeBase, QARecordReq req) {
        KnowledgeBaseQa newRecord = new KnowledgeBaseQa();
        newRecord.setAiModelId(aiModelService.getIdByName(req.getModelName()));
        newRecord.setQuestion(req.getQuestion());
        newRecord.setKbId(knowledgeBase.getId());
        newRecord.setKbUuid((knowledgeBase.getUuid()));
        newRecord.setUuid(UuidUtil.createShort());
        newRecord.setUserId(ThreadContext.getCurrentUserId());
        baseMapper.insert(newRecord);

        KbQaDto result = new KbQaDto();
        BeanUtils.copyProperties(newRecord, result);
        return result;
    }

    public Page<KbQaDto> search(String kbUuid, String keyword, Integer currentPage, Integer pageSize) {
        LambdaQueryWrapper<KnowledgeBaseQa> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(KnowledgeBaseQa::getKbUuid, kbUuid);
        wrapper.eq(KnowledgeBaseQa::getIsDeleted, false);
        if (Boolean.FALSE.equals(ThreadContext.getCurrentUser().getIsAdmin())) {
            wrapper.eq(KnowledgeBaseQa::getUserId, ThreadContext.getCurrentUserId());
        }
        if (StringUtils.isNotBlank(keyword)) {
            wrapper.like(KnowledgeBaseQa::getQuestion, keyword);
        }
        wrapper.orderByDesc(KnowledgeBaseQa::getUpdateTime);
        Page<KnowledgeBaseQa> page = baseMapper.selectPage(new Page<>(currentPage, pageSize), wrapper);

        Page<KbQaDto> result = new Page<>();
        MPPageUtil.convertToPage(page, result, KbQaDto.class, (t1, t2) -> {
            AiModel aiModel = MODEL_ID_TO_OBJ.get(t1.getAiModelId());
            t2.setAiModelPlatform(null == aiModel ? "" : aiModel.getPlatform());
            return t2;
        });
        return result;
    }

    /**
     * 增加嵌入引用记录
     *
     * @param user
     * @param qaRecordId       qa记录id
     * @param embeddingToScore
     */
    public void createEmbeddingRefs(User user, Long qaRecordId, Map<String, Double> embeddingToScore) {
        log.info("更新向量引用,userId:{},qaRecordId:{},embeddingToScore.size:{}", user.getId(), qaRecordId, embeddingToScore.size());
        for (Map.Entry<String, Double> entry : embeddingToScore.entrySet()) {
            String embeddingId = entry.getKey();
            KnowledgeBaseQaRefEmbedding recordReference = new KnowledgeBaseQaRefEmbedding();
            recordReference.setQaRecordId(qaRecordId);
            recordReference.setEmbeddingId(embeddingId);
            recordReference.setScore(embeddingToScore.get(embeddingId));
            recordReference.setUserId(user.getId());
            knowledgeBaseQaRecordReferenceService.save(recordReference);
        }
    }

    /**
     * 增加图谱引用记录
     *
     * @param user
     * @param qaRecordId
     * @param graphDto
     */
    public void createGraphRefs(User user, Long qaRecordId, KbQaRefGraphDto graphDto) {
        log.info("更新图谱引用,userId:{},qaRecordId:{},vertices.Size:{},edges.size:{}", user.getId(), qaRecordId, graphDto.getVertices().size(), graphDto.getEdges().size());
        String entities = null == graphDto.getEntitiesFromLlm() ? "" : String.join(",", graphDto.getEntitiesFromLlm());
        Map<String, Object> graphFromStore = new HashMap<>();
        graphFromStore.put("vertices", graphDto.getVertices());
        graphFromStore.put("edges", graphDto.getEdges());
        KnowledgeBaseQaRefGraph refGraph = new KnowledgeBaseQaRefGraph();
        refGraph.setQaRecordId(qaRecordId);
        refGraph.setUserId(user.getId());
        refGraph.setGraphFromLlm(entities);
        refGraph.setGraphFromStore(JsonUtil.toJson(graphFromStore));
        knowledgeBaseQaRecordRefGraphService.save(refGraph);
    }

    public List<KbQaRefEmbeddingDto> listReferences(String uuid) {
        List<KnowledgeBaseQaRefEmbedding> recordReferences = knowledgeBaseQaRecordReferenceService.listByQaUuid(uuid);
        if (CollectionUtils.isEmpty(recordReferences)) {
            return Collections.emptyList();
        }
        List<String> embeddingIds = recordReferences.stream().map(KnowledgeBaseQaRefEmbedding::getEmbeddingId).toList();
        if (CollectionUtils.isEmpty(embeddingIds)) {
            return Collections.emptyList();
        }
        List<KbItemEmbeddingDto> embeddings = iEmbeddingService.listByEmbeddingIds(embeddingIds);
        List<KbQaRefEmbeddingDto> result = new ArrayList<>();
        for (KbItemEmbeddingDto embedding : embeddings) {
            KbQaRefEmbeddingDto newOne = KbQaRefEmbeddingDto.builder()
                    .embeddingId(embedding.getEmbeddingId())
                    .text(embedding.getText())
                    .build();
            result.add(newOne);
        }
        return result;
    }

    public KnowledgeBaseQa getOrThrow(String uuid) {
        KnowledgeBaseQa exist = ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(KnowledgeBaseQa::getUuid, uuid)
                .eq(KnowledgeBaseQa::getIsDeleted, false)
                .one();
        if (null == exist) {
            throw new BaseException(A_DATA_NOT_FOUND);
        }
        return exist;
    }

    public void clearByCurrentUser() {
        ChainWrappers.lambdaUpdateChain(baseMapper)
                .eq(KnowledgeBaseQa::getUserId, ThreadContext.getCurrentUserId())
                .set(KnowledgeBaseQa::getIsDeleted, true)
                .update();
    }

    public boolean softDelete(String uuid) {
        if (Boolean.TRUE.equals(ThreadContext.getCurrentUser().getIsAdmin())) {
            return ChainWrappers.lambdaUpdateChain(baseMapper)
                    .eq(KnowledgeBaseQa::getUuid, uuid)
                    .set(KnowledgeBaseQa::getIsDeleted, true)
                    .update();
        }
        KnowledgeBaseQa exist = ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(KnowledgeBaseQa::getUuid, uuid)
                .one();
        if (null == exist) {
            throw new BaseException(A_DATA_NOT_FOUND);
        }
        return ChainWrappers.lambdaUpdateChain(baseMapper)
                .eq(KnowledgeBaseQa::getId, exist.getId())
                .set(KnowledgeBaseQa::getIsDeleted, true)
                .update();
    }
}

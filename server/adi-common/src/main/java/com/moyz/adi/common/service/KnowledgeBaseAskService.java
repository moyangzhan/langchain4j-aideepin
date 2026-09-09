package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.baomidou.mybatisplus.extension.toolkit.ChainWrappers;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.KbAskDto;
import com.moyz.adi.common.dto.KbAskReq;
import com.moyz.adi.common.dto.RefGraphDto;
import com.moyz.adi.common.entity.*;
import com.moyz.adi.common.enums.LLMCallRecordSourceType;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.mapper.KnowledgeBaseAskMapper;
import com.moyz.adi.common.util.EntityRefreshUtil;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.util.MPPageUtil;
import com.moyz.adi.common.util.UuidUtil;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static com.moyz.adi.common.enums.ErrorEnum.A_DATA_NOT_FOUND;
import static com.moyz.adi.common.util.LocalCache.MODEL_ID_TO_OBJ;

@Slf4j
@Service
public class KnowledgeBaseAskService extends ServiceImpl<KnowledgeBaseAskMapper, KnowledgeBaseAsk> {

    @Resource
    private KnowledgeBaseAskRefEmbeddingService knowledgeBaseAskRefEmbeddingService;

    @Resource
    private KnowledgeBaseAskRefGraphService knowledgeBaseAskRefGraphService;

    @Resource
    private AiModelService aiModelService;

    @Resource
    private LLMCallRecordService llmCallRecordService;

    public KbAskDto add(KnowledgeBase knowledgeBase, KbAskReq req) {
        KnowledgeBaseAsk newRecord = new KnowledgeBaseAsk();
        newRecord.setAiModelId(aiModelService.getIdByName(req.getModelName()));
        newRecord.setQuestion(req.getQuestion());
        newRecord.setKbId(knowledgeBase.getId());
        newRecord.setKbUuid((knowledgeBase.getUuid()));
        newRecord.setUuid(UuidUtil.createShort());
        newRecord.setUserId(ThreadContext.getCurrentUserId());
        baseMapper.insert(newRecord);

        // Re-read the row so database-managed columns (create_time / update_time / is_deleted)
        // are populated before copying into the DTO.
        KnowledgeBaseAsk fresh = EntityRefreshUtil.refresh(baseMapper, newRecord);

        KbAskDto result = new KbAskDto();
        BeanUtils.copyProperties(fresh, result);
        return result;
    }

    public Page<KbAskDto> search(String kbUuid, String keyword, Integer currentPage, Integer pageSize) {
        LambdaQueryWrapper<KnowledgeBaseAsk> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(KnowledgeBaseAsk::getKbUuid, kbUuid);
        wrapper.eq(KnowledgeBaseAsk::getIsDeleted, false);
        if (Boolean.FALSE.equals(ThreadContext.getCurrentUser().getIsAdmin())) {
            wrapper.eq(KnowledgeBaseAsk::getUserId, ThreadContext.getCurrentUserId());
        }
        if (StringUtils.isNotBlank(keyword)) {
            wrapper.like(KnowledgeBaseAsk::getQuestion, keyword);
        }
        wrapper.orderByDesc(KnowledgeBaseAsk::getUpdateTime);
        Page<KnowledgeBaseAsk> page = baseMapper.selectPage(new Page<>(currentPage, pageSize), wrapper);

        // Batch query LLM call records for token and duration
        List<Long> askIds = page.getRecords().stream().map(KnowledgeBaseAsk::getId).toList();
        Map<Long, LLMCallRecord> idToCallRecord = Map.of();
        if (!askIds.isEmpty()) {
            idToCallRecord = llmCallRecordService.listBySource(
                    LLMCallRecordSourceType.KNOWLEDGE_BASE_QA.getValue(), askIds)
                    .stream().collect(Collectors.toMap(LLMCallRecord::getSourceId, r -> r, (a, b) -> a));
        }

        Map<Long, LLMCallRecord> finalIdToCallRecord = idToCallRecord;
        Page<KbAskDto> result = new Page<>();
        MPPageUtil.convertToPage(page, result, KbAskDto.class, (t1, t2) -> {
            AiModel aiModel = MODEL_ID_TO_OBJ.get(t1.getAiModelId());
            t2.setAiModelPlatform(null == aiModel ? "" : aiModel.getPlatform());
            // Fill token and duration from LLM call record
            LLMCallRecord callRecord = finalIdToCallRecord.get(t1.getId());
            if (callRecord != null) {
                t2.setInputTokens(callRecord.getInputTokens());
                t2.setOutputTokens(callRecord.getOutputTokens());
                t2.setDuration(callRecord.getDuration());
            }
            return t2;
        });
        return result;
    }

    /**
     * 增加嵌入引用记录
     *
     * @param user
     * @param askRecordId       qa记录id
     * @param embeddingToScore
     */
    public void createEmbeddingRefs(User user, Long askRecordId, Map<String, Double> embeddingToScore) {
        log.info("Updating vector reference, userId:{}, askRecordId:{}, embeddingToScore.size:{}", user.getId(), askRecordId, embeddingToScore.size());
        for (Map.Entry<String, Double> entry : embeddingToScore.entrySet()) {
            String embeddingId = entry.getKey();
            KnowledgeBaseAskRefEmbedding recordReference = new KnowledgeBaseAskRefEmbedding();
            recordReference.setAskRecordId(askRecordId);
            recordReference.setEmbeddingId(embeddingId);
            recordReference.setScore(embeddingToScore.get(embeddingId));
            recordReference.setUserId(user.getId());
            knowledgeBaseAskRefEmbeddingService.save(recordReference);
        }
    }

    /**
     * 增加图谱引用记录
     *
     * @param user
     * @param askRecordId
     * @param graphDto
     */
    public void createGraphRefs(User user, Long askRecordId, RefGraphDto graphDto) {
        log.info("Updating graph reference, userId:{}, askRecordId:{}, vertices.Size:{}, edges.size:{}", user.getId(), askRecordId, graphDto.getVertices().size(), graphDto.getEdges().size());
        String entities = null == graphDto.getEntitiesFromQuestion() ? "" : String.join(",", graphDto.getEntitiesFromQuestion());
        Map<String, Object> graphFromStore = new HashMap<>();
        graphFromStore.put("vertices", graphDto.getVertices());
        graphFromStore.put("edges", graphDto.getEdges());
        KnowledgeBaseAskRefGraph refGraph = new KnowledgeBaseAskRefGraph();
        refGraph.setAskRecordId(askRecordId);
        refGraph.setUserId(user.getId());
        refGraph.setEntitiesFromQuestion(entities);
        refGraph.setGraphFromStore(JsonUtil.toJson(graphFromStore));
        knowledgeBaseAskRefGraphService.save(refGraph);
    }

    public KnowledgeBaseAsk getOrThrow(String uuid) {
        KnowledgeBaseAsk exist = ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(KnowledgeBaseAsk::getUuid, uuid)
                .eq(KnowledgeBaseAsk::getIsDeleted, false)
                .one();
        if (null == exist) {
            throw new BaseException(A_DATA_NOT_FOUND);
        }
        return exist;
    }

    public void clearByCurrentUser() {
        ChainWrappers.lambdaUpdateChain(baseMapper)
                .eq(KnowledgeBaseAsk::getUserId, ThreadContext.getCurrentUserId())
                .set(KnowledgeBaseAsk::getIsDeleted, true)
                .update();
    }

    public boolean softDelete(String uuid) {
        if (Boolean.TRUE.equals(ThreadContext.getCurrentUser().getIsAdmin())) {
            return ChainWrappers.lambdaUpdateChain(baseMapper)
                    .eq(KnowledgeBaseAsk::getUuid, uuid)
                    .set(KnowledgeBaseAsk::getIsDeleted, true)
                    .update();
        }
        KnowledgeBaseAsk exist = ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(KnowledgeBaseAsk::getUuid, uuid)
                .one();
        if (null == exist) {
            throw new BaseException(A_DATA_NOT_FOUND);
        }
        return ChainWrappers.lambdaUpdateChain(baseMapper)
                .eq(KnowledgeBaseAsk::getId, exist.getId())
                .set(KnowledgeBaseAsk::getIsDeleted, true)
                .update();
    }
}

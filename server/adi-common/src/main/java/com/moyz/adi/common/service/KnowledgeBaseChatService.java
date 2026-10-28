package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.baomidou.mybatisplus.extension.toolkit.ChainWrappers;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.KbChatDto;
import com.moyz.adi.common.dto.KbChatReq;
import com.moyz.adi.common.dto.RefGraphDto;
import com.moyz.adi.common.entity.*;
import com.moyz.adi.common.enums.LLMCallRecordSourceType;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.mapper.KnowledgeBaseChatMapper;
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
public class KnowledgeBaseChatService extends ServiceImpl<KnowledgeBaseChatMapper, KnowledgeBaseChat> {

    @Resource
    private KnowledgeBaseChatRefEmbeddingService knowledgeBaseChatRefEmbeddingService;

    @Resource
    private KnowledgeBaseChatRefGraphService knowledgeBaseChatRefGraphService;

    @Resource
    private AiModelService aiModelService;

    @Resource
    private LLMCallRecordService llmCallRecordService;

    public KbChatDto add(KnowledgeBase knowledgeBase, KbChatReq req) {
        KnowledgeBaseChat newRecord = new KnowledgeBaseChat();
        newRecord.setAiModelId(aiModelService.getIdByName(req.getModelName()));
        newRecord.setQuestion(req.getQuestion());
        newRecord.setKbId(knowledgeBase.getId());
        newRecord.setKbUuid((knowledgeBase.getUuid()));
        newRecord.setUuid(UuidUtil.createShort());
        newRecord.setUserId(ThreadContext.getCurrentUserId());
        baseMapper.insert(newRecord);

        // Re-read the row so database-managed columns (create_time / update_time / is_deleted)
        // are populated before copying into the DTO.
        KnowledgeBaseChat fresh = EntityRefreshUtil.refresh(baseMapper, newRecord);

        KbChatDto result = new KbChatDto();
        BeanUtils.copyProperties(fresh, result);
        return result;
    }

    public Page<KbChatDto> search(String kbUuid, String keyword, Integer currentPage, Integer pageSize) {
        LambdaQueryWrapper<KnowledgeBaseChat> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(KnowledgeBaseChat::getKbUuid, kbUuid);
        wrapper.eq(KnowledgeBaseChat::getIsDeleted, false);
        if (Boolean.FALSE.equals(ThreadContext.getCurrentUser().getIsAdmin())) {
            wrapper.eq(KnowledgeBaseChat::getUserId, ThreadContext.getCurrentUserId());
        }
        if (StringUtils.isNotBlank(keyword)) {
            wrapper.like(KnowledgeBaseChat::getQuestion, keyword);
        }
        wrapper.orderByDesc(KnowledgeBaseChat::getUpdateTime);
        Page<KnowledgeBaseChat> page = baseMapper.selectPage(new Page<>(currentPage, pageSize), wrapper);

        // Batch query LLM call records for token and duration
        List<Long> chatIds = page.getRecords().stream().map(KnowledgeBaseChat::getId).toList();
        Map<Long, LLMCallRecord> idToCallRecord = Map.of();
        if (!chatIds.isEmpty()) {
            idToCallRecord = llmCallRecordService.listBySource(
                    LLMCallRecordSourceType.KNOWLEDGE_BASE_QA.getValue(), chatIds)
                    .stream().collect(Collectors.toMap(LLMCallRecord::getSourceId, r -> r, (a, b) -> a));
        }

        Map<Long, LLMCallRecord> finalIdToCallRecord = idToCallRecord;
        Page<KbChatDto> result = new Page<>();
        MPPageUtil.convertToPage(page, result, KbChatDto.class, (t1, t2) -> {
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
     * @param chatRecordId       qa记录id
     * @param embeddingToScore
     */
    public void createEmbeddingRefs(User user, Long chatRecordId, Map<String, Double> embeddingToScore) {
        log.info("Updating vector reference, userId:{}, chatRecordId:{}, embeddingToScore.size:{}", user.getId(), chatRecordId, embeddingToScore.size());
        for (Map.Entry<String, Double> entry : embeddingToScore.entrySet()) {
            String embeddingId = entry.getKey();
            KnowledgeBaseChatRefEmbedding recordReference = new KnowledgeBaseChatRefEmbedding();
            recordReference.setChatRecordId(chatRecordId);
            recordReference.setEmbeddingId(embeddingId);
            recordReference.setScore(embeddingToScore.get(embeddingId));
            recordReference.setUserId(user.getId());
            knowledgeBaseChatRefEmbeddingService.save(recordReference);
        }
    }

    /**
     * 增加图谱引用记录
     *
     * @param user
     * @param chatRecordId
     * @param graphDto
     */
    public void createGraphRefs(User user, Long chatRecordId, RefGraphDto graphDto) {
        log.info("Updating graph reference, userId:{}, chatRecordId:{}, vertices.Size:{}, edges.size:{}", user.getId(), chatRecordId, graphDto.getVertices().size(), graphDto.getEdges().size());
        String entities = null == graphDto.getEntitiesFromQuestion() ? "" : String.join(",", graphDto.getEntitiesFromQuestion());
        Map<String, Object> graphFromStore = new HashMap<>();
        graphFromStore.put("vertices", graphDto.getVertices());
        graphFromStore.put("edges", graphDto.getEdges());
        KnowledgeBaseChatRefGraph refGraph = new KnowledgeBaseChatRefGraph();
        refGraph.setChatRecordId(chatRecordId);
        refGraph.setUserId(user.getId());
        refGraph.setEntitiesFromQuestion(entities);
        refGraph.setGraphFromStore(JsonUtil.toJson(graphFromStore));
        knowledgeBaseChatRefGraphService.save(refGraph);
    }

    public KnowledgeBaseChat getOrThrow(String uuid) {
        KnowledgeBaseChat exist = ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(KnowledgeBaseChat::getUuid, uuid)
                .eq(KnowledgeBaseChat::getIsDeleted, false)
                .one();
        if (null == exist) {
            throw new BaseException(A_DATA_NOT_FOUND);
        }
        return exist;
    }

    public void clearByCurrentUser() {
        ChainWrappers.lambdaUpdateChain(baseMapper)
                .eq(KnowledgeBaseChat::getUserId, ThreadContext.getCurrentUserId())
                .set(KnowledgeBaseChat::getIsDeleted, true)
                .update();
    }

    public boolean softDelete(String uuid) {
        if (Boolean.TRUE.equals(ThreadContext.getCurrentUser().getIsAdmin())) {
            return ChainWrappers.lambdaUpdateChain(baseMapper)
                    .eq(KnowledgeBaseChat::getUuid, uuid)
                    .set(KnowledgeBaseChat::getIsDeleted, true)
                    .update();
        }
        KnowledgeBaseChat exist = ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(KnowledgeBaseChat::getUuid, uuid)
                .one();
        if (null == exist) {
            throw new BaseException(A_DATA_NOT_FOUND);
        }
        return ChainWrappers.lambdaUpdateChain(baseMapper)
                .eq(KnowledgeBaseChat::getId, exist.getId())
                .set(KnowledgeBaseChat::getIsDeleted, true)
                .update();
    }
}

package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.RefGraphDto;
import com.moyz.adi.common.entity.CharacterMessage;
import com.moyz.adi.common.entity.CharacterMessageRefEmbedding;
import com.moyz.adi.common.entity.CharacterMessageRefGraph;
import com.moyz.adi.common.entity.CharacterMessageRefMemoryEmbedding;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.mapper.CharacterMessageMapper;
import com.moyz.adi.common.rag.AdiEmbeddingStoreContentRetriever;
import com.moyz.adi.common.rag.GraphStoreContentRetriever;
import com.moyz.adi.common.util.JsonUtil;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.stereotype.Service;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.moyz.adi.common.enums.ErrorEnum.B_MESSAGE_NOT_FOUND;

/**
 * Message CRUD service: pure database operations, no chat orchestration logic.
 */
@Slf4j
@Service
public class CharacterMessageService extends ServiceImpl<CharacterMessageMapper, CharacterMessage> {

    @Resource
    private CharacterMessageRefEmbeddingService characterMessageRefEmbeddingService;

    @Resource
    private CharacterMessageRefGraphService characterMessageRefGraphService;

    @Resource
    private CharacterMessageRefMemoryEmbeddingService characterMessageRefMemoryEmbeddingService;

    public List<CharacterMessage> listQuestionsByCharacterId(long characterId, long maxId, int pageSize) {
        LambdaQueryWrapper<CharacterMessage> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(CharacterMessage::getCharacterId, characterId);
        queryWrapper.eq(CharacterMessage::getParentMessageId, 0);
        queryWrapper.lt(CharacterMessage::getId, maxId);
        queryWrapper.eq(CharacterMessage::getIsDeleted, false);
        queryWrapper.last("limit " + pageSize);
        queryWrapper.orderByDesc(CharacterMessage::getId);
        return getBaseMapper().selectList(queryWrapper);
    }

    public boolean softDelete(String uuid) {
        return this.lambdaUpdate()
                .eq(CharacterMessage::getUuid, uuid)
                .eq(CharacterMessage::getUserId, ThreadContext.getCurrentUserId())
                .eq(CharacterMessage::getIsDeleted, false)
                .set(CharacterMessage::getIsDeleted, true)
                .update();
    }

    public String getTextByAudioUuid(String audioUuid) {
        if (audioUuid == null || audioUuid.isBlank()) {
            return null;
        }
        CharacterMessage conversationMessage = this.lambdaQuery()
                .eq(CharacterMessage::getAudioUuid, audioUuid)
                .eq(!ThreadContext.getCurrentUser().getIsAdmin(), CharacterMessage::getUserId, ThreadContext.getCurrentUserId())
                .eq(CharacterMessage::getIsDeleted, false)
                .last("limit 1")
                .oneOpt()
                .orElse(null);
        if (null == conversationMessage) {
            return null;
        }
        return conversationMessage.getRemark();
    }

    /**
     * 增加嵌入引用记录
     *
     * @param user             用户
     * @param messageId        消息id
     * @param embeddingToScore 嵌入向量id和分数的映射
     */
    public void createEmbeddingRefs(User user, Long messageId, Map<String, Double> embeddingToScore) {
        log.info("Creating vector reference, userId:{}, qaRecordId:{}, embeddingToScore.size:{}", user.getId(), messageId, embeddingToScore.size());
        for (Map.Entry<String, Double> entry : embeddingToScore.entrySet()) {
            String embeddingId = entry.getKey();
            CharacterMessageRefEmbedding recordReference = new CharacterMessageRefEmbedding();
            recordReference.setMessageId(messageId);
            recordReference.setEmbeddingId(embeddingId);
            recordReference.setScore(embeddingToScore.get(embeddingId));
            recordReference.setUserId(user.getId());
            characterMessageRefEmbeddingService.save(recordReference);
        }
    }

    public void createMemoryRefs(User user, Long messageId, Map<String, Double> embeddingToScore) {
        log.info("Creating memory vector reference, userId:{}, qaRecordId:{}, embeddingToScore.size:{}", user.getId(), messageId, embeddingToScore.size());
        for (Map.Entry<String, Double> entry : embeddingToScore.entrySet()) {
            String embeddingId = entry.getKey();
            CharacterMessageRefMemoryEmbedding refEmb = new CharacterMessageRefMemoryEmbedding();
            refEmb.setMessageId(messageId);
            refEmb.setEmbeddingId(embeddingId);
            refEmb.setScore(embeddingToScore.get(embeddingId));
            refEmb.setUserId(user.getId());
            characterMessageRefMemoryEmbeddingService.save(refEmb);
        }
    }

    /**
     * 增加图谱引用记录
     *
     * @param user      用户
     * @param messageId 消息id
     * @param graphDto  图谱引用数据传输对象
     */
    public void createGraphRefs(User user, Long messageId, RefGraphDto graphDto) {
        log.info("Preparing to create graph reference, userId:{}, qaRecordId:{}, vertices.Size:{}, edges.size:{}", user.getId(), messageId, graphDto.getVertices().size(), graphDto.getEdges().size());
        if (graphDto.getVertices().isEmpty() && graphDto.getEdges().isEmpty()) {
            log.warn("Graph reference data is empty, cannot create graph reference record, userId:{}, qaRecordId:{}", user.getId(), messageId);
            return;
        }
        String entities = null == graphDto.getEntitiesFromQuestion() ? "" : String.join(",", graphDto.getEntitiesFromQuestion());
        Map<String, Object> graphFromStore = new HashMap<>();
        graphFromStore.put("vertices", graphDto.getVertices());
        graphFromStore.put("edges", graphDto.getEdges());
        CharacterMessageRefGraph refGraph = new CharacterMessageRefGraph();
        refGraph.setMessageId(messageId);
        refGraph.setUserId(user.getId());
        refGraph.setGraphFromLlm(entities);
        refGraph.setGraphFromStore(JsonUtil.toJson(graphFromStore));
        characterMessageRefGraphService.save(refGraph);
    }
}

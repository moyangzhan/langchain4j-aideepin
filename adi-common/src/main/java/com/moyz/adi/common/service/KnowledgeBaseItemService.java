package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.baomidou.mybatisplus.extension.toolkit.ChainWrappers;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.dto.KbItemDto;
import com.moyz.adi.common.dto.KbItemEditReq;
import com.moyz.adi.common.entity.KnowledgeBase;
import com.moyz.adi.common.entity.KnowledgeBaseItem;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.enums.EmbeddingStatusEnum;
import com.moyz.adi.common.enums.GraphicalStatusEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.helper.LLMContext;
import com.moyz.adi.common.interfaces.AbstractLLMService;
import com.moyz.adi.common.mapper.KnowledgeBaseItemMapper;
import com.moyz.adi.common.rag.CompositeRAG;
import com.moyz.adi.common.util.UuidUtil;
import com.moyz.adi.common.vo.GraphIngestParams;
import com.moyz.adi.common.vo.LLMBuilderProperties;
import dev.langchain4j.data.document.Document;
import dev.langchain4j.data.document.Metadata;
import dev.langchain4j.model.chat.ChatLanguageModel;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.formula.functions.T;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.text.MessageFormat;
import java.time.LocalDateTime;
import java.util.List;
import java.util.concurrent.TimeUnit;

import static com.moyz.adi.common.cosntant.AdiConstant.DOC_INDEX_TYPE_EMBEDDING;
import static com.moyz.adi.common.cosntant.AdiConstant.DOC_INDEX_TYPE_GRAPHICAL;
import static com.moyz.adi.common.cosntant.RedisKeyConstant.KB_STATISTIC_RECALCULATE_SIGNAL;
import static com.moyz.adi.common.cosntant.RedisKeyConstant.USER_INDEXING;
import static com.moyz.adi.common.enums.ErrorEnum.*;

@Slf4j
@Service
public class KnowledgeBaseItemService extends ServiceImpl<KnowledgeBaseItemMapper, KnowledgeBaseItem> {

    @Resource
    @Lazy
    private KnowledgeBaseItemService self;

    @Resource
    private StringRedisTemplate stringRedisTemplate;

    @Resource
    private CompositeRAG compositeRAG;

    @Resource
    private KnowledgeBaseEmbeddingService knowledgeBaseEmbeddingService;

    public KnowledgeBaseItem saveOrUpdate(KbItemEditReq itemEditReq) {
        String uuid = itemEditReq.getUuid();
        KnowledgeBaseItem item = new KnowledgeBaseItem();
        item.setTitle(itemEditReq.getTitle());
        if (StringUtils.isNotBlank(itemEditReq.getBrief())) {
            item.setBrief(itemEditReq.getBrief());
        } else {
            item.setBrief(StringUtils.substring(itemEditReq.getRemark(), 0, 200));
        }
        item.setRemark(itemEditReq.getRemark());
        if (null == itemEditReq.getId() || itemEditReq.getId() < 1) {
            uuid = UuidUtil.createShort();
            item.setUuid(uuid);
            item.setKbId(itemEditReq.getKbId());
            item.setKbUuid(itemEditReq.getKbUuid());
            baseMapper.insert(item);
        } else {
            item.setId(itemEditReq.getId());
            baseMapper.updateById(item);
        }

        stringRedisTemplate.opsForSet().add(KB_STATISTIC_RECALCULATE_SIGNAL, itemEditReq.getKbUuid());

        return ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(KnowledgeBaseItem::getUuid, uuid)
                .one();
    }

    public KnowledgeBaseItem getEnable(String uuid) {
        return ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(KnowledgeBaseItem::getUuid, uuid)
                .eq(KnowledgeBaseItem::getIsDeleted, false)
                .one();
    }

    public Page<KbItemDto> search(String kbUuid, String keyword, Integer currentPage, Integer pageSize) {
        return baseMapper.searchByKb(new Page<>(currentPage, pageSize), kbUuid, keyword);
    }

    /**
     * 批量索引知识点
     *
     * @param knowledgeBase 知识库
     * @param kbItemUuids   知识点uuid列表
     * @param indexTypes 索引类型，如embedding,graphical
     * @return 成功或失败
     */
    public boolean checkAndIndexing(KnowledgeBase knowledgeBase, List<String> kbItemUuids, List<String> indexTypes) {
        for (String kbItemUuid : kbItemUuids) {
            if (checkPrivilege(kbItemUuid)) {
                KnowledgeBaseItem item = getEnable(kbItemUuid);
                self.asyncIndex(ThreadContext.getCurrentUser(), knowledgeBase, item, indexTypes);
            }
        }
        return true;
    }

    /**
     * 对文档进行索引(向量化、图谱化)
     *
     * @param user          用户
     * @param knowledgeBase 知识库
     * @param kbItem        知识点
     * @param indexTypes    索引类型，如embedding,graphical
     */
    @Async
    public void asyncIndex(User user, KnowledgeBase knowledgeBase, KnowledgeBaseItem kbItem, List<String> indexTypes) {
        stringRedisTemplate.opsForValue().set(MessageFormat.format(USER_INDEXING, knowledgeBase.getOwnerId()), "", 10, TimeUnit.MINUTES);
        try {
            if (indexTypes.contains(DOC_INDEX_TYPE_EMBEDDING) && kbItem.getEmbeddingStatus() != EmbeddingStatusEnum.DOING) {
                Metadata metadata = new Metadata();
                metadata.put(AdiConstant.MetadataKey.KB_UUID, kbItem.getKbUuid());
                metadata.put(AdiConstant.MetadataKey.KB_ITEM_UUID, kbItem.getUuid());
                Document document = new Document(kbItem.getRemark(), metadata);
                knowledgeBaseEmbeddingService.deleteByItemUuid(kbItem.getUuid());
                indexingEmbedding(knowledgeBase, kbItem, document);
            }
            if (indexTypes.contains(DOC_INDEX_TYPE_GRAPHICAL) && kbItem.getGraphicalStatus() != GraphicalStatusEnum.DOING) {
                Metadata metadata = new Metadata();
                metadata.put(AdiConstant.MetadataKey.KB_UUID, kbItem.getKbUuid());
                metadata.put(AdiConstant.MetadataKey.KB_ITEM_UUID, kbItem.getUuid());
                Document document = new Document(kbItem.getRemark(), metadata);
                indexingGraph(user, knowledgeBase, kbItem, document);
            }
        } finally {
            stringRedisTemplate.opsForSet().add(KB_STATISTIC_RECALCULATE_SIGNAL, kbItem.getKbUuid());
            stringRedisTemplate.delete(MessageFormat.format(USER_INDEXING, knowledgeBase.getOwnerId()));
        }

    }

    private void indexingEmbedding(KnowledgeBase knowledgeBase, KnowledgeBaseItem kbItem, Document document) {
        try {
            ChainWrappers.lambdaUpdateChain(baseMapper)
                    .eq(KnowledgeBaseItem::getId, kbItem.getId())
                    .set(KnowledgeBaseItem::getEmbeddingStatusChangeTime, LocalDateTime.now())
                    .set(KnowledgeBaseItem::getEmbeddingStatus, EmbeddingStatusEnum.DOING)
                    .update();
            compositeRAG.getEmbeddingRAGService().ingest(document, knowledgeBase.getIngestMaxOverlap(), null);
            ChainWrappers.lambdaUpdateChain(baseMapper)
                    .eq(KnowledgeBaseItem::getId, kbItem.getId())
                    .set(KnowledgeBaseItem::getEmbeddingStatus, EmbeddingStatusEnum.DONE)
                    .update();
        } catch (Exception e) {
            log.error("ingestForEmbedding error", e);
            ChainWrappers.lambdaUpdateChain(baseMapper)
                    .eq(KnowledgeBaseItem::getId, kbItem.getId())
                    .set(KnowledgeBaseItem::getEmbeddingStatusChangeTime, LocalDateTime.now())
                    .set(KnowledgeBaseItem::getEmbeddingStatus, EmbeddingStatusEnum.FAIL)
                    .update();
        }
    }

    private void indexingGraph(User user, KnowledgeBase knowledgeBase, KnowledgeBaseItem kbItem, Document document) {
        try {
            ChainWrappers.lambdaUpdateChain(baseMapper)
                    .eq(KnowledgeBaseItem::getId, kbItem.getId())
                    .set(KnowledgeBaseItem::getGraphicalStatusChangeTime, LocalDateTime.now())
                    .set(KnowledgeBaseItem::getGraphicalStatus, GraphicalStatusEnum.DOING)
                    .update();
            AbstractLLMService<T> llmService = LLMContext.getLLMServiceById(knowledgeBase.getIngestModelId());
            ChatLanguageModel chatLanguageModel = llmService.buildChatLLM(
                    LLMBuilderProperties.builder()
                            .temperature(knowledgeBase.getQueryLlmTemperature())
                            .build()
                    , kbItem.getUuid()
            );

            //Ingest document
            compositeRAG.getGraphRAGService().ingest(
                    GraphIngestParams.builder()
                            .user(user)
                            .document(document)
                            .overlap(knowledgeBase.getIngestMaxOverlap())
                            .chatLanguageModel(chatLanguageModel)
                            .identifyColumns(List.of(AdiConstant.MetadataKey.KB_UUID))
                            .appendColumns(List.of(AdiConstant.MetadataKey.KB_ITEM_UUID))
                            .isFreeToken(llmService.getAiModel().getIsFree())
                            .build()
            );
            ChainWrappers.lambdaUpdateChain(baseMapper)
                    .eq(KnowledgeBaseItem::getId, kbItem.getId())
                    .set(KnowledgeBaseItem::getGraphicalStatus, GraphicalStatusEnum.DONE)
                    .update();
        } catch (Exception e) {
            log.error("ingestForGraph error", e);
            ChainWrappers.lambdaUpdateChain(baseMapper)
                    .eq(KnowledgeBaseItem::getId, kbItem.getId())
                    .set(KnowledgeBaseItem::getGraphicalStatusChangeTime, LocalDateTime.now())
                    .set(KnowledgeBaseItem::getGraphicalStatus, GraphicalStatusEnum.FAIL)
                    .update();
        }
    }

    @Transactional
    public boolean softDelete(String uuid) {
        boolean privilege = checkPrivilege(uuid);
        if (!privilege) throw new BaseException(A_USER_NOT_AUTH);
        boolean success = ChainWrappers.lambdaUpdateChain(baseMapper)
                .eq(KnowledgeBaseItem::getUuid, uuid)
                .set(KnowledgeBaseItem::getIsDeleted, true)
                .update();
        if (!success) {
            return false;
        }
        knowledgeBaseEmbeddingService.deleteByItemUuid(uuid);

        KnowledgeBaseItem item = baseMapper.getByUuid(uuid);
        if (null != item) {
            stringRedisTemplate.opsForSet().add(KB_STATISTIC_RECALCULATE_SIGNAL, item.getKbUuid());
        }
        return true;
    }

    public int countByKbUuid(String kbUuid) {
        return ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(KnowledgeBaseItem::getKbUuid, kbUuid)
                .eq(KnowledgeBaseItem::getIsDeleted, false)
                .count()
                .intValue();
    }

    public int countTodayCreated() {
        LocalDateTime now = LocalDateTime.now();
        LocalDateTime beginTime = LocalDateTime.of(now.getYear(), now.getMonth(), now.getDayOfMonth(), 0, 0, 0);
        LocalDateTime endTime = beginTime.plusDays(1);
        return baseMapper.countCreatedByTimePeriod(beginTime, endTime);
    }

    public int countAllCreated() {
        return baseMapper.countAllCreated();
    }

    private boolean checkPrivilege(String uuid) {
        if (StringUtils.isBlank(uuid)) {
            throw new BaseException(A_PARAMS_ERROR);
        }
        User user = ThreadContext.getCurrentUser();
        if (null == user) {
            throw new BaseException(A_USER_NOT_EXIST);
        }
        if (Boolean.TRUE.equals(user.getIsAdmin())) {
            return true;
        }
        int belongToUser = baseMapper.belongToUser(uuid, user.getId());
        return belongToUser > 0;
    }
}

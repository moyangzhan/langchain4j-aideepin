package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.baomidou.mybatisplus.extension.toolkit.ChainWrappers;
import com.baomidou.mybatisplus.core.toolkit.support.SFunction;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.dto.KbDocumentDto;
import com.moyz.adi.common.dto.KbDocumentEditReq;
import com.moyz.adi.common.entity.KnowledgeBase;
import com.moyz.adi.common.entity.DocumentSegment;
import com.moyz.adi.common.entity.KbDocument;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.enums.EmbeddingStatusEnum;
import com.moyz.adi.common.enums.GraphicalStatusEnum;
import com.moyz.adi.common.enums.SegmentModeEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.helper.LLMContext;
import com.moyz.adi.common.mapper.KbDocumentMapper;
import com.moyz.adi.common.rag.GraphRagContext;
import com.moyz.adi.common.service.embedding.IKnowledgeEmbeddingService;
import com.moyz.adi.common.languagemodel.AbstractLLMService;
import com.moyz.adi.common.util.UuidUtil;
import com.moyz.adi.common.vo.ChatModelBuilderProperties;
import com.moyz.adi.common.vo.GraphIngestParam;
import dev.langchain4j.model.chat.ChatModel;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
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
import static com.moyz.adi.common.cosntant.AdiConstant.RetrieveContentFrom.KNOWLEDGE_BASE;
import static com.moyz.adi.common.cosntant.RedisKeyConstant.KB_STATISTIC_RECALCULATE_SIGNAL;
import static com.moyz.adi.common.cosntant.RedisKeyConstant.USER_INDEXING;
import static com.moyz.adi.common.enums.ErrorEnum.*;

@Slf4j
@Service
public class KbDocumentService extends ServiceImpl<KbDocumentMapper, KbDocument> {

    @Resource
    @Lazy
    private KbDocumentService self;

    @Resource
    private StringRedisTemplate stringRedisTemplate;

    @Resource
    private IKnowledgeEmbeddingService iKnowledgeEmbeddingService;

    @Resource
    private SegmentIndexService segmentIndexService;

    @Resource
    private DocumentSegmentService documentSegmentService;

    @Resource
    private KnowledgeBaseGraphService knowledgeBaseGraphService;

    @Resource
    private FileService fileService;

    public KbDocument saveOrUpdate(KbDocumentEditReq itemEditReq) {
        String uuid = itemEditReq.getUuid();
        // Authorize before mutating: by knowledge-base uuid when creating (no item
        // uuid exists yet), by item id when updating (the client-controlled uuid
        // cannot be trusted; the real target is resolved by id).
        if (null == itemEditReq.getId() || itemEditReq.getId() < 1) {
            checkWritePrivilegeByKb(itemEditReq.getKbUuid());
        } else {
            checkWritePrivilegeById(itemEditReq.getId());
        }
        KbDocument item = new KbDocument();
        item.setTitle(itemEditReq.getTitle());
        if (StringUtils.isNotBlank(itemEditReq.getBrief())) {
            item.setBrief(itemEditReq.getBrief());
        } else {
            item.setBrief(StringUtils.substring(itemEditReq.getRemark(), 0, 200));
        }
        item.setRemark(itemEditReq.getRemark());
        // 分段模式（文档级）：未指定时按 text 处理；改动模式后需重新索引才生效
        item.setSegmentMode(itemEditReq.getSegmentMode() == null ? SegmentModeEnum.TEXT : itemEditReq.getSegmentMode());
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
                .eq(KbDocument::getUuid, uuid)
                .one();
    }

    public KbDocument getEnable(String uuid) {
        return ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(KbDocument::getUuid, uuid)
                .eq(KbDocument::getIsDeleted, false)
                .one();
    }

    /**
     * Search items in a knowledge base by keyword.
     */
    public Page<KbDocumentDto> search(String kbUuid, String keyword, Integer currentPage, Integer pageSize) {
        Page<KbDocumentDto> page = baseMapper.searchByKb(new Page<>(currentPage, pageSize), kbUuid, keyword);
        page.getRecords().forEach(item -> item.setSourceFileUrl(fileService.getUrl(item.getSourceFileUuid())));
        return page;
    }

    /**
     * 批量索引知识点
     *
     * @param knowledgeBase 知识库
     * @param kbItemUuids   知识点uuid列表
     * @param indexTypes    索引类型，如embedding,graphical
     * @return 成功或失败
     */
    public boolean checkAndIndexing(KnowledgeBase knowledgeBase, List<String> kbItemUuids, List<String> indexTypes) {
        String userIndexKey = MessageFormat.format(USER_INDEXING, knowledgeBase.getOwnerId());
        boolean hasTask = false;
        for (String kbItemUuid : kbItemUuids) {
            if (hasWritePrivilege(kbItemUuid)) {
                KbDocument item = getEnable(kbItemUuid);
                if (item != null) {
                    if (!hasTask) {
                        stringRedisTemplate.opsForValue().set(userIndexKey, "0", 10, TimeUnit.MINUTES);
                        hasTask = true;
                    }
                    self.asyncIndex(ThreadContext.getCurrentUser(), knowledgeBase, item, indexTypes);
                }
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
    public void asyncIndex(User user, KnowledgeBase knowledgeBase, KbDocument kbItem, List<String> indexTypes) {
        String userIndexKey = MessageFormat.format(USER_INDEXING, knowledgeBase.getOwnerId());
        stringRedisTemplate.opsForValue().increment(userIndexKey);
        stringRedisTemplate.expire(userIndexKey, 10, TimeUnit.MINUTES);
        try {
            if (indexTypes.contains(DOC_INDEX_TYPE_EMBEDDING) && kbItem.getEmbeddingStatus() != EmbeddingStatusEnum.DOING) {
                indexingEmbedding(knowledgeBase, kbItem);
            }
            if (indexTypes.contains(DOC_INDEX_TYPE_GRAPHICAL) && kbItem.getGraphicalStatus() != GraphicalStatusEnum.DOING) {
                indexingGraph(user, knowledgeBase, kbItem);
            }
        } finally {
            stringRedisTemplate.opsForSet().add(KB_STATISTIC_RECALCULATE_SIGNAL, kbItem.getKbUuid());
            Long remaining = stringRedisTemplate.opsForValue().decrement(userIndexKey);
            if (remaining != null && remaining <= 0) {
                stringRedisTemplate.delete(userIndexKey);
            }
        }

    }

    private void indexingEmbedding(KnowledgeBase knowledgeBase, KbDocument kbItem) {
        try {
            ChainWrappers.lambdaUpdateChain(baseMapper)
                    .eq(KbDocument::getId, kbItem.getId())
                    .set(KbDocument::getEmbeddingStatusChangeTime, LocalDateTime.now())
                    .set(KbDocument::getEmbeddingStatus, EmbeddingStatusEnum.DOING)
                    .update();
            // 切段显式化 + 按模式向量化（text 主表行 / qa 问题行 / parent_child 子块行）
            segmentIndexService.reindexEmbedding(knowledgeBase, kbItem);
            ChainWrappers.lambdaUpdateChain(baseMapper)
                    .eq(KbDocument::getId, kbItem.getId())
                    .set(KbDocument::getEmbeddingStatus, EmbeddingStatusEnum.DONE)
                    .update();
        } catch (Exception e) {
            log.error("ingestForEmbedding error", e);
            ChainWrappers.lambdaUpdateChain(baseMapper)
                    .eq(KbDocument::getId, kbItem.getId())
                    .set(KbDocument::getEmbeddingStatusChangeTime, LocalDateTime.now())
                    .set(KbDocument::getEmbeddingStatus, EmbeddingStatusEnum.FAIL)
                    .update();
        }
    }

    private void indexingGraph(User user, KnowledgeBase knowledgeBase, KbDocument kbItem) {
        try {
            ChainWrappers.lambdaUpdateChain(baseMapper)
                    .eq(KbDocument::getId, kbItem.getId())
                    .set(KbDocument::getGraphicalStatusChangeTime, LocalDateTime.now())
                    .set(KbDocument::getGraphicalStatus, GraphicalStatusEnum.DOING)
                    .update();
            AbstractLLMService llmService = LLMContext.getServiceById(knowledgeBase.getIngestModelId(), true);
            ChatModel ChatModel = llmService.buildChatLLM(
                    ChatModelBuilderProperties.builder()
                            .temperature(knowledgeBase.getQueryLlmTemperature())
                            .build()
            );

            // 先清后抽：按账本清理该文档图谱足迹（幂等），重跑从"追加合并"变为"先清后建"——
            // 同时充当存量文档的懒迁移入口与漂移修复入口；停用段不参与重抽
            knowledgeBaseGraphService.removeDocumentGraphFootprint(knowledgeBase.getUuid(), kbItem.getUuid());
            List<DocumentSegment> segments = segmentIndexService.ensureSegments(knowledgeBase, kbItem).stream()
                    .filter(segment -> !Boolean.FALSE.equals(segment.getIsEnabled()))
                    .toList();
            GraphRagContext.get(KNOWLEDGE_BASE).ingest(
                    GraphIngestParam.builder()
                            .user(user)
                            .segments(segments)
                            .ChatModel(ChatModel)
                            .identifyColumns(List.of(AdiConstant.MetadataKey.KB_UUID))
                            .appendColumns(List.of(AdiConstant.MetadataKey.KB_ITEM_UUID))
                            .isFreeToken(llmService.getAiModel().getIsFree())
                            .sourceId(kbItem.getId())
                            .modelPlatform(llmService.getAiModel().getPlatform())
                            .modelName(llmService.getAiModel().getName())
                            .build()
            );
            ChainWrappers.lambdaUpdateChain(baseMapper)
                    .eq(KbDocument::getId, kbItem.getId())
                    .set(KbDocument::getGraphicalStatus, GraphicalStatusEnum.DONE)
                    .update();
        } catch (Exception e) {
            log.error("ingestForGraph error", e);
            ChainWrappers.lambdaUpdateChain(baseMapper)
                    .eq(KbDocument::getId, kbItem.getId())
                    .set(KbDocument::getGraphicalStatusChangeTime, LocalDateTime.now())
                    .set(KbDocument::getGraphicalStatus, GraphicalStatusEnum.FAIL)
                    .update();
        }
    }

    @Transactional
    public boolean softDelete(String uuid) {
        checkWritePrivilege(uuid);
        boolean success = ChainWrappers.lambdaUpdateChain(baseMapper)
                .eq(KbDocument::getUuid, uuid)
                .set(KbDocument::getIsDeleted, true)
                .update();
        if (!success) {
            return false;
        }
        iKnowledgeEmbeddingService.deleteByItemUuid(uuid);
        documentSegmentService.deleteByDocUuid(uuid);

        KbDocument item = baseMapper.getByUuid(uuid);
        if (null != item) {
            // 补齐存量缺口：文档删除时清理其图谱足迹（账本驱动，文档外无贡献者的元素定点删除）。
            // 尽力而为：图库异常不应阻断文档删除，残留可由该库后续图谱操作收敛
            try {
                knowledgeBaseGraphService.removeDocumentGraphFootprint(item.getKbUuid(), uuid);
            } catch (Exception e) {
                log.error("Remove document graph footprint failed, docUuid:{}", uuid, e);
            }
            stringRedisTemplate.opsForSet().add(KB_STATISTIC_RECALCULATE_SIGNAL, item.getKbUuid());
        }
        return true;
    }

    /**
     * 单段索引重建（启用分段时异步调用）：向量重嵌与图谱重抽两路独立执行、各自更新段级状态
     * （embedding_status / graphical_status，失败标 FAIL，前端可对已启用段重复调用启停幂等重试）。
     * 图谱路径先按账本幂等清理该段残留（防重复追加）再抽取，账本行随 ingest 双写重建。
     */
    @Async
    public void asyncRebuildSegment(User user, KnowledgeBase knowledgeBase, KbDocument kbItem, DocumentSegment segment) {
        String userIndexKey = MessageFormat.format(USER_INDEXING, knowledgeBase.getOwnerId());
        stringRedisTemplate.opsForValue().increment(userIndexKey);
        stringRedisTemplate.expire(userIndexKey, 10, TimeUnit.MINUTES);
        try {
            try {
                segmentIndexService.vectorizeSegment(knowledgeBase, kbItem, segment);
                updateSegmentIndexStatus(segment.getId(), DocumentSegment::getEmbeddingStatus, EmbeddingStatusEnum.DONE);
            } catch (Exception e) {
                log.error("Rebuild segment embedding error, segmentUuid:{}", segment.getUuid(), e);
                updateSegmentIndexStatus(segment.getId(), DocumentSegment::getEmbeddingStatus, EmbeddingStatusEnum.FAIL);
            }
            if (kbItem.getGraphicalStatus() == GraphicalStatusEnum.DONE) {
                try {
                    knowledgeBaseGraphService.removeSegmentGraphFootprint(knowledgeBase.getUuid(), segment.getUuid());
                    AbstractLLMService llmService = LLMContext.getServiceById(knowledgeBase.getIngestModelId(), true);
                    ChatModel chatModel = llmService.buildChatLLM(
                            ChatModelBuilderProperties.builder()
                                    .temperature(knowledgeBase.getQueryLlmTemperature())
                                    .build()
                    );
                    GraphRagContext.get(KNOWLEDGE_BASE).ingest(
                            GraphIngestParam.builder()
                                    .user(user)
                                    .segments(List.of(segment))
                                    .ChatModel(chatModel)
                                    .identifyColumns(List.of(AdiConstant.MetadataKey.KB_UUID))
                                    .appendColumns(List.of(AdiConstant.MetadataKey.KB_ITEM_UUID))
                                    .isFreeToken(llmService.getAiModel().getIsFree())
                                    .sourceId(kbItem.getId())
                                    .modelPlatform(llmService.getAiModel().getPlatform())
                                    .modelName(llmService.getAiModel().getName())
                                    .build()
                    );
                    updateSegmentIndexStatus(segment.getId(), DocumentSegment::getGraphicalStatus, GraphicalStatusEnum.DONE);
                } catch (Exception e) {
                    log.error("Rebuild segment graph error, segmentUuid:{}", segment.getUuid(), e);
                    updateSegmentIndexStatus(segment.getId(), DocumentSegment::getGraphicalStatus, GraphicalStatusEnum.FAIL);
                }
            }
        } finally {
            Long remaining = stringRedisTemplate.opsForValue().decrement(userIndexKey);
            if (remaining != null && remaining <= 0) {
                stringRedisTemplate.delete(userIndexKey);
            }
        }
    }

    private void updateSegmentIndexStatus(Long segmentId,
                                          SFunction<DocumentSegment, ?> column,
                                          Object status) {
        documentSegmentService.lambdaUpdate()
                .eq(DocumentSegment::getId, segmentId)
                .set(column, status)
                .update();
    }

    public int countByKbUuid(String kbUuid) {
        return ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(KbDocument::getKbUuid, kbUuid)
                .eq(KbDocument::getIsDeleted, false)
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

    /**
     * Toggle the enabled/disabled status of a document. When disabled, the document's
     * segments are excluded from vector and graph retrieval.
     */
    public boolean toggleStatus(String uuid, Boolean isEnabled) {
        checkWritePrivilege(uuid);
        return ChainWrappers.lambdaUpdateChain(baseMapper)
                .eq(KbDocument::getUuid, uuid)
                .set(KbDocument::getIsEnabled, isEnabled)
                .set(KbDocument::getEnabledChangeTime, LocalDateTime.now())
                .update();
    }

    /**
     * List the UUIDs of disabled documents in a knowledge base, used to exclude
     * their segments from retrieval.
     */
    public List<String> listDisabledItemUuids(String kbUuid) {
        return baseMapper.listDisabledItemUuids(kbUuid);
    }

    public void incrementEmbeddingHitCount(List<String> uuids) {
        if (uuids != null && !uuids.isEmpty()) {
            baseMapper.incrementEmbeddingHitCount(uuids);
        }
    }

    public void incrementGraphHitCount(List<String> uuids) {
        if (uuids != null && !uuids.isEmpty()) {
            baseMapper.incrementGraphHitCount(uuids);
        }
    }

    /**
     * Fetch a knowledge-base item by uuid after a read-privilege check. Both the
     * check and the query operate on the item itself, so they are kept together
     * here rather than split across the controller.
     */
    public KbDocument info(String uuid) {
        checkReadPrivilege(uuid);
        return getEnable(uuid);
    }

    /**
     * Write-privilege check: allows the owner or an admin. Throws
     * {@link com.moyz.adi.common.enums.ErrorEnum#A_USER_NOT_AUTH} on denial.
     * Used by single write operations such as delete.
     */
    public void checkWritePrivilege(String uuid) {
        if (!hasWritePrivilege(uuid)) {
            throw new BaseException(A_USER_NOT_AUTH);
        }
    }

    /**
     * Write-privilege probe: returns whether the owner or an admin may write,
     * without throwing. Used by batch flows (e.g. indexing) that skip items the
     * current user is not allowed to touch instead of aborting the whole batch.
     */
    public boolean hasWritePrivilege(String uuid) {
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
        return baseMapper.checkWritePrivilege(uuid, user.getId()) > 0;
    }

    /**
     * Write-privilege check keyed by knowledge-base uuid: allows the owner of the
     * knowledge base or an admin. Throws
     * {@link com.moyz.adi.common.enums.ErrorEnum#A_USER_NOT_AUTH} on denial.
     * Used when creating a new item, where no item uuid exists yet.
     */
    public void checkWritePrivilegeByKb(String kbUuid) {
        if (!hasWritePrivilegeByKb(kbUuid)) {
            throw new BaseException(A_USER_NOT_AUTH);
        }
    }

    /**
     * Write-privilege probe keyed by knowledge-base uuid: returns whether the
     * owner of the knowledge base or an admin may write, without throwing.
     */
    public boolean hasWritePrivilegeByKb(String kbUuid) {
        if (StringUtils.isBlank(kbUuid)) {
            throw new BaseException(A_PARAMS_ERROR);
        }
        User user = ThreadContext.getCurrentUser();
        if (null == user) {
            throw new BaseException(A_USER_NOT_EXIST);
        }
        if (Boolean.TRUE.equals(user.getIsAdmin())) {
            return true;
        }
        return baseMapper.checkWritePrivilegeByKb(kbUuid, user.getId()) > 0;
    }

    /**
     * Write-privilege check keyed by item id: allows the owner of the item's
     * knowledge base or an admin. Throws
     * {@link com.moyz.adi.common.enums.ErrorEnum#A_USER_NOT_AUTH} on denial.
     * Used when updating an item, where the real target is resolved by id rather
     * than the client-controlled uuid.
     */
    public void checkWritePrivilegeById(Long id) {
        if (!hasWritePrivilegeById(id)) {
            throw new BaseException(A_USER_NOT_AUTH);
        }
    }

    /**
     * Write-privilege probe keyed by item id: returns whether the owner of the
     * item's knowledge base or an admin may write, without throwing.
     */
    public boolean hasWritePrivilegeById(Long id) {
        if (null == id || id < 1) {
            throw new BaseException(A_PARAMS_ERROR);
        }
        User user = ThreadContext.getCurrentUser();
        if (null == user) {
            throw new BaseException(A_USER_NOT_EXIST);
        }
        if (Boolean.TRUE.equals(user.getIsAdmin())) {
            return true;
        }
        return baseMapper.checkWritePrivilegeById(id, user.getId()) > 0;
    }

    /**
     * Read-privilege check: allows the owner, an admin, or anyone when the
     * owning knowledge base is public. Used by reads of an item and its derived
     * content (embeddings, graph). Denials are reported as
     * {@link com.moyz.adi.common.enums.ErrorEnum#A_DATA_NOT_FOUND} to avoid
     * leaking the existence of other users' private knowledge bases.
     */
    public void checkReadPrivilege(String uuid) {
        if (StringUtils.isBlank(uuid)) {
            throw new BaseException(A_PARAMS_ERROR);
        }
        User user = ThreadContext.getCurrentUser();
        if (null == user) {
            throw new BaseException(A_USER_NOT_EXIST);
        }
        if (Boolean.TRUE.equals(user.getIsAdmin())) {
            return;
        }
        if (baseMapper.checkReadPrivilege(uuid, user.getId()) == 0) {
            throw new BaseException(A_DATA_NOT_FOUND);
        }
    }
}

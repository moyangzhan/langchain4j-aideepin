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
import java.util.Objects;
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
    private IndexTaskService indexTaskService;

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
            KbDocument old = baseMapper.selectById(itemEditReq.getId());
            item.setId(itemEditReq.getId());
            baseMapper.updateById(item);
            invalidateSegmentsIfChanged(old, itemEditReq, uuid);
        }

        stringRedisTemplate.opsForSet().add(KB_STATISTIC_RECALCULATE_SIGNAL, itemEditReq.getKbUuid());

        return ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(KbDocument::getUuid, uuid)
                .one();
    }

    /**
     * "变更即失效"模型：段行是 remark 的物化，内容变了段行即过期，下次重新向量化自动走
     * 切段分支重建（全量、新段默认启用）。qa 模式的段行来自问答数据流、与 remark 无关，
     * 仅 remark 变更不失效（模式切换仍失效，切换即放弃旧结构）。
     * 顺带修复存量不一致：旧实现在重跑索引前，旧向量仍按旧内容命中检索。
     * 图谱足迹不动——文档编辑从不触发图谱变更，图谱重跑自带先清后抽。
     */
    private void invalidateSegmentsIfChanged(KbDocument old, KbDocumentEditReq req, String docUuid) {
        if (old == null) {
            return;
        }
        SegmentModeEnum oldMode = old.getSegmentMode() == null ? SegmentModeEnum.TEXT : old.getSegmentMode();
        SegmentModeEnum newMode = req.getSegmentMode() == null ? SegmentModeEnum.TEXT : req.getSegmentMode();
        boolean modeChanged = newMode != oldMode;
        boolean remarkChanged = !Objects.equals(old.getRemark(), req.getRemark());
        if (!modeChanged && !(remarkChanged && newMode != SegmentModeEnum.QA)) {
            return;
        }
        // “变更即失效”+自动重索引：版本推进使既有索引过期（在途任务经检查点作废并自清理）。
        // 清理仅在无 running 任务时立即执行，否则移交在途任务的取消善后——清理与写入单线程化
        bumpIndexVersion(docUuid);
        if (!indexTaskService.hasRunningByDoc(docUuid)) {
            iKnowledgeEmbeddingService.deleteByItemUuid(docUuid);
            documentSegmentService.deleteByDocUuid(docUuid);
        }
        markEmbeddingPending(docUuid);
        // 自动触发仅向量化；图谱始终手动（LLM 昂贵，频繁保存不应反复抽取）
        indexTaskService.enqueueDocument(old.getKbUuid(), docUuid, DOC_INDEX_TYPE_EMBEDDING, ThreadContext.getCurrentUser());
    }

    /**
     * 索引版本原子推进：任何使既有索引过期的变更调用（竞态检测与合并去抖的信号源）
     */
    private void bumpIndexVersion(String docUuid) {
        ChainWrappers.lambdaUpdateChain(baseMapper)
                .eq(KbDocument::getUuid, docUuid)
                .setSql("index_version = index_version + 1")
                .update();
    }

    /**
     * KB 切段参数变更后失效该库下所有可切段文档的段行与向量（qa 文档除外——其段行来自问答数据流），
     * 下次重新向量化按新参数切段重建。
     */
    public void invalidateSegmentsByKb(String kbUuid) {
        List<KbDocument> docs = ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(KbDocument::getKbUuid, kbUuid)
                .eq(KbDocument::getIsDeleted, false)
                .list();
        for (KbDocument doc : docs) {
            if (SegmentIndexService.effectiveMode(doc) == SegmentModeEnum.QA) {
                continue;
            }
            bumpIndexVersion(doc.getUuid());
            if (!indexTaskService.hasRunningByDoc(doc.getUuid())) {
                iKnowledgeEmbeddingService.deleteByItemUuid(doc.getUuid());
                documentSegmentService.deleteByDocUuid(doc.getUuid());
            }
            markEmbeddingPending(doc.getUuid());
            indexTaskService.enqueueDocument(doc.getKbUuid(), doc.getUuid(), DOC_INDEX_TYPE_EMBEDDING, ThreadContext.getCurrentUser());
        }
    }

    /**
     * 段行失效后文档级向量化状态归 NONE，前端文档列表正确显示"待向量化"
     */
    private void markEmbeddingPending(String docUuid) {
        ChainWrappers.lambdaUpdateChain(baseMapper)
                .eq(KbDocument::getUuid, docUuid)
                .set(KbDocument::getEmbeddingStatus, EmbeddingStatusEnum.NONE)
                .update();
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
                    User user = ThreadContext.getCurrentUser();
                    for (String indexType : indexTypes) {
                        indexTaskService.enqueueDocument(knowledgeBase.getUuid(), kbItemUuid, indexType, user);
                    }
                }
            }
        }
        return true;
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

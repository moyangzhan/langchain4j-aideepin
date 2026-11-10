package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.spring.service.impl.ServiceImpl;
import com.baomidou.mybatisplus.extension.toolkit.ChainWrappers;
import com.baomidou.mybatisplus.core.toolkit.support.SFunction;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.dto.KbDocumentAttachmentDto;
import com.moyz.adi.common.dto.KbDocumentDto;
import com.moyz.adi.common.dto.KbDocumentEditReq;
import com.moyz.adi.common.entity.AdiFile;
import com.moyz.adi.common.entity.KnowledgeBase;
import com.moyz.adi.common.entity.DocumentSegment;
import com.moyz.adi.common.entity.KbDocument;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.enums.EmbeddingStatusEnum;
import com.moyz.adi.common.enums.GraphicalStatusEnum;
import com.moyz.adi.common.enums.SegmentModeEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.file.FileOperatorContext;
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
import org.springframework.transaction.support.TransactionTemplate;

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
    private TransactionTemplate transactionTemplate;

    @Resource
    private StringRedisTemplate stringRedisTemplate;

    // @Lazy: documentQaModeService -> kbDocumentService -> 本服务,成环;生成入口仅在保存后调用
    @Lazy
    @Resource
    private DocumentQaModeService documentQaModeService;

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
    private com.moyz.adi.common.mapper.KnowledgeBaseMapper knowledgeBaseMapper;

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
        // 分段模式（文档级）：未指定时按 text 处理；模式变更走"变更即失效"并自动重索引（仅 embedding，图谱始终手动）
        item.setSegmentMode(itemEditReq.getSegmentMode() == null ? SegmentModeEnum.TEXT : itemEditReq.getSegmentMode());
        // 子块最大token数（文档级，父子模式专用）；未传时不覆盖已有值，新增时落库默认值
        if (itemEditReq.getChildMaxChunkSize() != null) {
            Integer childSize = itemEditReq.getChildMaxChunkSize();
            if (childSize < AdiConstant.ChildChunkSize.MIN || childSize > AdiConstant.ChildChunkSize.MAX) {
                throw new BaseException(A_PARAMS_ERROR);
            }
            item.setChildMaxChunkSize(childSize);
        }
        if (null == itemEditReq.getId() || itemEditReq.getId() < 1) {
            // kbId is derived server-side from the authorized KB: a client-controlled kb_id
            // could diverge from kb_uuid and poison every privilege join (they join on kb_id)
            KnowledgeBase kb = knowledgeBaseMapper.selectOne(new com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper<KnowledgeBase>()
                    .eq(KnowledgeBase::getUuid, itemEditReq.getKbUuid())
                    .eq(KnowledgeBase::getIsDeleted, false));
            if (kb == null) {
                throw new BaseException(A_DATA_NOT_FOUND);
            }
            uuid = UuidUtil.createShort();
            item.setUuid(uuid);
            item.setKbId(kb.getId());
            item.setKbUuid(kb.getUuid());
            baseMapper.insert(item);
        } else {
            KbDocument old = baseMapper.selectById(itemEditReq.getId());
            // the row resolved by id is authoritative: every side effect below keys on it,
            // a client uuid that disagrees must be rejected instead of trusted
            if (old == null || !Objects.equals(old.getUuid(), itemEditReq.getUuid())) {
                throw new BaseException(A_DATA_NOT_FOUND);
            }
            uuid = old.getUuid();
            item.setId(itemEditReq.getId());
            baseMapper.updateById(item);
            invalidateSegmentsIfChanged(old, itemEditReq, uuid);
        }

        KbDocument saved = ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(KbDocument::getUuid, uuid)
                .one();
        if (null != saved) {
            stringRedisTemplate.opsForSet().add(KB_STATISTIC_RECALCULATE_SIGNAL, saved.getKbUuid());
        }
        // 保存为 qa 模式且勾选自动生成：无段行直接生成，已有问答对替换式重新生成（清空后重建）
        if (null != saved && saved.getSegmentMode() == SegmentModeEnum.QA
                && Boolean.TRUE.equals(itemEditReq.getAutoGenerateQa())) {
            documentQaModeService.autoGenerateQa(ThreadContext.getCurrentUser(), saved);
        }
        return saved;
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
        // 子块大小是父子模式的切段参数，变更即失效该文档段行（文档级，不影响库内其他文档）
        boolean childSizeChanged = req.getChildMaxChunkSize() != null
                && newMode == SegmentModeEnum.PARENT_CHILD
                && !Objects.equals(old.getChildMaxChunkSize(), req.getChildMaxChunkSize());
        if (!modeChanged && !childSizeChanged && !(remarkChanged && newMode != SegmentModeEnum.QA)) {
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
        // 自动触发仅向量化；图谱始终手动（LLM 昂贵，频繁保存不应反复抽取）。
        // 切到 qa 且勾选自动生成时跳过：紧随其后的 autoGenerateQa 自己置 DOING、
        // 生成并向量化，再入队 embedding 任务会与生成流程并发写同一文档
        if (newMode == SegmentModeEnum.QA && Boolean.TRUE.equals(req.getAutoGenerateQa())) {
            return;
        }
        indexTaskService.enqueueDocument(old.getKbUuid(), docUuid, DOC_INDEX_TYPE_EMBEDDING, ThreadContext.getCurrentUser());
    }

    /**
     * 文档级全量重建：入队指定维度的索引任务，不改动分段内容。
     * embedding=按现有分段重新向量化（无分段的文档补切；问答文档重嵌其问题）；graphical=图谱重抽（任务自带先清后抽）。
     */
    public boolean rebuildDoc(String docUuid, String indexType) {
        checkWritePrivilege(docUuid);
        KbDocument doc = getEnable(docUuid);
        if (doc == null) {
            throw new BaseException(A_DATA_NOT_FOUND);
        }
        if (EmbeddingStatusEnum.DOING == doc.getEmbeddingStatus()
                || GraphicalStatusEnum.DOING == doc.getGraphicalStatus()
                || indexTaskService.hasRunningByDoc(docUuid)) {
            throw new BaseException(A_DOC_INDEX_DOING);
        }
        if (!DOC_INDEX_TYPE_EMBEDDING.equals(indexType) && !DOC_INDEX_TYPE_GRAPHICAL.equals(indexType)) {
            throw new BaseException(A_PARAMS_ERROR);
        }
        indexTaskService.enqueueDocument(doc.getKbUuid(), docUuid, indexType, ThreadContext.getCurrentUser());
        return true;
    }

    /**
     * 替换式 QA 重新生成的清理前置：版本推进（在途任务经检查点自作废）+ 段行/问题/子块
     * 软删 + 向量清理。仅在调用方确认无在跑任务后调用——清理需立即生效，不能像模式切换
     * 那样移交在途任务的取消善后（否则段行非空的窗口会让生成入口误判跳过）。
     */
    public void clearSegmentsForQaRegenerate(String docUuid) {
        bumpIndexVersion(docUuid);
        iKnowledgeEmbeddingService.deleteByItemUuid(docUuid);
        documentSegmentService.deleteByDocUuid(docUuid);
        markEmbeddingPending(docUuid);
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
        // one batched lookup instead of a per-row query (N+1 on large pages)
        List<String> fileUuids = page.getRecords().stream()
                .map(KbDocumentDto::getSourceFileUuid)
                .filter(StringUtils::isNotBlank)
                .toList();
        java.util.Map<String, String> urlByUuid = fileService.getUrlByUuids(fileUuids);
        page.getRecords().forEach(item -> item.setSourceFileUrl(urlByUuid.get(item.getSourceFileUuid())));
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
                    // seed only when absent: a plain SET would clobber the live counter the
                    // task executor maintains with increment/decrement while its tasks run
                    stringRedisTemplate.opsForValue().setIfAbsent(userIndexKey, "0", 10, TimeUnit.MINUTES);
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

    /**
     * 批量软删：逐个走单删管线；已删或不存在的文档，管理员静默跳过、普通用户抛无权限异常，中断时已删的保持生效。
     */
    public void softDeleteBatch(List<String> uuids) {
        for (String uuid : uuids) {
            softDelete(uuid);
        }
    }

    /**
     * 删除单个文档的完整管线，1-4 阶段收在一个事务里：
     * 1、软删文档行并解除源文件引用；
     * 2、取消索引任务，在途任务在检查点作废；
     * 3、软删全部段行；
     * 4、软删段派生的问题与子块；
     * 5、清理向量与图谱足迹、发统计重算信号。
     * 阶段 5 为远程调用，在事务外执行。
     * <p>
     * Full pipeline for deleting one document; stages 1-4 sit in one transaction:
     * 1. soft-delete the document row and detach its source-file reference;
     * 2. cancel index tasks (in-flight tasks abort at their next checkpoint);
     * 3. soft-delete all segment rows;
     * 4. soft-delete segment-derived questions and child chunks;
     * 5. clean up vectors and the graph footprint, then signal a statistic recalculation.
     * Stage 5 makes remote calls and runs outside the transaction.
     * </p>
     */
    public boolean softDelete(String uuid) {
        checkWritePrivilege(uuid);
        Boolean deleted = transactionTemplate.execute(status -> {
            if (!softDeleteDocument(uuid)) {
                return false;
            }
            cancelIndexTasks(uuid);
            softDeleteSegments(uuid);
            softDeleteSegmentDerivedData(uuid);
            return true;
        });
        if (!Boolean.TRUE.equals(deleted)) {
            return false;
        }
        cleanupAfterDelete(uuid);
        return true;
    }

    /**
     * 条件软删文档行，成功后解除其源文件引用。
     * Conditionally soft-delete the document row, then detach its source-file reference.
     */
    private boolean softDeleteDocument(String uuid) {
        KbDocument document = baseMapper.getByUuid(uuid);
        boolean success = ChainWrappers.lambdaUpdateChain(baseMapper)
                .eq(KbDocument::getUuid, uuid)
                .eq(KbDocument::getIsDeleted, false)
                .set(KbDocument::getIsDeleted, true)
                .update();
        if (!success) {
            return false;
        }
        if (null != document && null != document.getSourceFileId()) {
            AdiFile sourceFile = fileService.getById(document.getSourceFileId());
            if (null != sourceFile) {
                fileService.detachRef(sourceFile.getUuid());
            }
        }
        return true;
    }

    private void cancelIndexTasks(String uuid) {
        indexTaskService.cancelByDoc(uuid);
    }

    private void softDeleteSegments(String uuid) {
        documentSegmentService.deleteSegmentsByDocUuid(uuid);
    }

    private void softDeleteSegmentDerivedData(String uuid) {
        documentSegmentService.deleteDerivedByDocUuid(uuid);
    }

    /**
     * 事务外清理，尽力而为：向量删除若失败，残留的向量不可达（段行已软删，检索展开排除），
     * 由该文档下次重建索引收敛；图库异常不阻断删除，残留由该库后续图谱操作收敛。
     * <p>
     * Best-effort cleanup outside the transaction: vectors left behind by a failed delete are
     * unreachable (their segment rows are soft-deleted and excluded from retrieval) and purged by
     * the next doc-level reindex; a graph-store failure never blocks deletion.
     * </p>
     */
    private void cleanupAfterDelete(String uuid) {
        iKnowledgeEmbeddingService.deleteByItemUuid(uuid);
        KbDocument item = baseMapper.getByUuid(uuid);
        if (null != item) {
            try {
                knowledgeBaseGraphService.removeDocumentGraphFootprint(item.getKbUuid(), uuid);
            } catch (Exception e) {
                log.error("Remove document graph footprint failed, docUuid:{}", uuid, e);
            }
            stringRedisTemplate.opsForSet().add(KB_STATISTIC_RECALCULATE_SIGNAL, item.getKbUuid());
        }
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
     * 失败重试：按文档当前失败维度与分段模式路由——qa 模式且尚无段行（生成失败）走 QA
     * 重新生成；其余按 embedding/graphical 失败维度重新入队文档级索引任务。前端展示入口
     * 挂状态列（FAIL），不依赖 fail_reason 是否有值
     */
    public boolean retryIndex(String uuid) {
        checkWritePrivilege(uuid);
        KbDocument doc = getEnable(uuid);
        if (null == doc) {
            throw new BaseException(A_DATA_NOT_FOUND);
        }
        if (EmbeddingStatusEnum.DOING == doc.getEmbeddingStatus()
                || GraphicalStatusEnum.DOING == doc.getGraphicalStatus()
                || indexTaskService.hasRunningByDoc(uuid)) {
            throw new BaseException(A_DOC_INDEX_DOING);
        }
        User user = ThreadContext.getCurrentUser();
        if (SegmentModeEnum.QA == doc.getSegmentMode()
                && documentSegmentService.listByDocUuid(uuid).isEmpty()) {
            documentQaModeService.autoGenerateQa(user, doc);
            return true;
        }
        boolean enqueued = false;
        if (EmbeddingStatusEnum.FAIL == doc.getEmbeddingStatus()) {
            indexTaskService.enqueueDocument(doc.getKbUuid(), uuid, DOC_INDEX_TYPE_EMBEDDING, user);
            enqueued = true;
        }
        if (GraphicalStatusEnum.FAIL == doc.getGraphicalStatus()) {
            indexTaskService.enqueueDocument(doc.getKbUuid(), uuid, DOC_INDEX_TYPE_GRAPHICAL, user);
            enqueued = true;
        }
        // DOING is set by the executor when it claims the task, not here; the frontend tells
        // queued from finally failed via /document/indexProgress
        return enqueued;
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
     * Source file of a file-converted document: display name plus access url.
     */
    public KbDocumentAttachmentDto getAttachment(String uuid) {
        checkReadPrivilege(uuid);
        KbDocument doc = getEnable(uuid);
        if (doc == null || doc.getSourceFileId() == null) {
            throw new BaseException(A_DATA_NOT_FOUND);
        }
        AdiFile file = fileService.getById(doc.getSourceFileId());
        if (file == null) {
            throw new BaseException(A_DATA_NOT_FOUND);
        }
        KbDocumentAttachmentDto dto = new KbDocumentAttachmentDto();
        dto.setName(StringUtils.isBlank(file.getName()) ? file.getUuid() + "." + file.getExt() : file.getName());
        dto.setUrl(FileOperatorContext.getFileUrl(file));
        return dto;
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

package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.core.toolkit.support.SFunction;
import com.baomidou.mybatisplus.extension.toolkit.ChainWrappers;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.DocumentSegment;
import com.moyz.adi.common.entity.IndexTask;
import com.moyz.adi.common.entity.KbDocument;
import com.moyz.adi.common.entity.KnowledgeBase;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.enums.EmbeddingStatusEnum;
import com.moyz.adi.common.enums.GraphicalStatusEnum;
import com.moyz.adi.common.exception.IndexTaskCancelledException;
import com.moyz.adi.common.helper.LLMContext;
import com.moyz.adi.common.languagemodel.AbstractLLMService;
import com.moyz.adi.common.mapper.IndexTaskMapper;
import com.moyz.adi.common.mapper.KbDocumentMapper;
import com.moyz.adi.common.mapper.KnowledgeBaseMapper;
import com.moyz.adi.common.rag.GraphRagContext;
import com.moyz.adi.common.service.embedding.IKnowledgeEmbeddingService;
import com.moyz.adi.common.vo.ChatModelBuilderProperties;
import com.moyz.adi.common.vo.GraphIngestParam;
import dev.langchain4j.model.chat.ChatModel;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.text.MessageFormat;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.TimeUnit;
import java.util.function.Supplier;

import static com.moyz.adi.common.cosntant.AdiConstant.DOC_INDEX_TYPE_EMBEDDING;
import static com.moyz.adi.common.cosntant.AdiConstant.DOC_INDEX_TYPE_GRAPHICAL;
import static com.moyz.adi.common.cosntant.RedisKeyConstant.KB_STATISTIC_RECALCULATE_SIGNAL;
import static com.moyz.adi.common.cosntant.RedisKeyConstant.USER_INDEXING;

/**
 * 索引任务队列：一切索引写入（切段/向量化/图谱抽取）的唯一通道。
 * <p>
 * 总原则：同 doc 任务串行（claimOne 领取互斥，advisory lock），跨 doc 并行；
 * 版本（index_version）承担竞态检测——任务携带入队时快照，结束条件置位，
 * 不匹配自动向最新版本重入队（合并去抖）；清理只发生在队列内安全点
 * （任务开头清 / 作废自清理），保存侧仅在"同 doc 无 running"时即时清理。
 * 图谱不自动触发（仅保存与段启用自动入队 embedding），失败仅手动重试。
 */
@Slf4j
@Service
public class IndexTaskService {

    public static final String TARGET_DOCUMENT = "document";
    public static final String TARGET_SEGMENT = "segment";
    public static final String STATUS_DONE = "done";
    public static final String STATUS_FAILED = "failed";

    private static final int STALE_RUNNING_MINUTES = 30;

    @Resource
    @Lazy
    private IndexTaskService self;

    @Resource
    private IndexTaskMapper indexTaskMapper;

    @Resource
    private KbDocumentMapper kbDocumentMapper;

    @Resource
    private KnowledgeBaseMapper knowledgeBaseMapper;

    @Resource
    private DocumentSegmentService documentSegmentService;

    @Resource
    private SegmentIndexService segmentIndexService;

    @Resource
    private KnowledgeBaseGraphService knowledgeBaseGraphService;

    @Resource
    private IKnowledgeEmbeddingService iKnowledgeEmbeddingService;

    @Resource
    private UserService userService;

    @Resource
    private StringRedisTemplate stringRedisTemplate;

    /**
     * 文档级任务入队（合并去抖：同键 pending 只更新版本与触发者）
     */
    public void enqueueDocument(String kbUuid, String docUuid, String taskType, User user) {
        KbDocument doc = kbDocumentMapper.getByUuid(docUuid);
        if (doc == null) {
            log.warn("enqueueDocument skipped, doc not found:{}", docUuid);
            return;
        }
        IndexTask task = new IndexTask();
        task.setKbUuid(kbUuid);
        task.setDocUuid(docUuid);
        task.setUserId(user.getId());
        task.setSegmentUuid("");
        task.setTargetType(TARGET_DOCUMENT);
        task.setTaskType(taskType);
        task.setVersion(doc.getIndexVersion() == null ? 0 : doc.getIndexVersion());
        indexTaskMapper.enqueue(task);
        self.dispatch();
    }

    /**
     * 段级任务入队（目标段必须存在）
     */
    public void enqueueSegment(KnowledgeBase kb, KbDocument doc, DocumentSegment segment, String taskType, User user) {
        IndexTask task = new IndexTask();
        task.setKbUuid(kb.getUuid());
        task.setDocUuid(doc.getUuid());
        task.setUserId(user.getId());
        task.setSegmentUuid(segment.getUuid());
        task.setTargetType(TARGET_SEGMENT);
        task.setTaskType(taskType);
        task.setVersion(segment.getIndexVersion() == null ? 0 : segment.getIndexVersion());
        indexTaskMapper.enqueue(task);
        self.dispatch();
    }

    /**
     * 该文档当前是否有 running 任务（保存侧决定即时清理还是移交清理）
     */
    public boolean hasRunningByDoc(String docUuid) {
        return indexTaskMapper.hasRunningByDoc(docUuid);
    }

    /**
     * 消费循环：领取-执行直到无任务可领。入队即触发；轮询兜底重触发。
     */
    @Async
    public void dispatch() {
        while (true) {
            IndexTask task = self.claimOne();
            if (task == null) {
                return;
            }
            execute(task);
        }
    }

    @Transactional
    public IndexTask claimOne() {
        return indexTaskMapper.claimOne();
    }

    /**
     * 轮询兜底：回收超时 running（进程崩溃遗留），重触发消费
     */
    @Scheduled(fixedDelay = 60_000)
    public void pollStale() {
        try {
            int reset = indexTaskMapper.resetStaleRunning(STALE_RUNNING_MINUTES);
            if (reset > 0) {
                log.warn("Reset {} stale running index tasks (> {} minutes)", reset, STALE_RUNNING_MINUTES);
                self.dispatch();
            }
        } catch (Exception e) {
            log.error("pollStale error", e);
        }
    }

    private void execute(IndexTask task) {
        String userIndexKey = MessageFormat.format(USER_INDEXING, task.getUserId());
        stringRedisTemplate.opsForValue().increment(userIndexKey);
        stringRedisTemplate.expire(userIndexKey, 10, TimeUnit.MINUTES);
        try {
            boolean skipped = route(task);
            indexTaskMapper.finishOne(task.getId(), STATUS_DONE, skipped ? "skipped: version advanced" : null);
        } catch (IndexTaskCancelledException e) {
            log.info("Index task cancelled, docUuid:{}, reason:{}", task.getDocUuid(), e.getMessage());
            onCancelled(task);
            indexTaskMapper.finishOne(task.getId(), STATUS_DONE, "skipped: version advanced");
        } catch (Exception e) {
            log.error("Index task failed, docUuid:{}, targetType:{}, taskType:{}",
                    task.getDocUuid(), task.getTargetType(), task.getTaskType(), e);
            indexTaskMapper.finishOne(task.getId(), STATUS_FAILED, StringUtils.abbreviate(e.getMessage(), 500));
        } finally {
            if (DOC_INDEX_TYPE_EMBEDDING.equals(task.getTaskType())) {
                stringRedisTemplate.opsForSet().add(KB_STATISTIC_RECALCULATE_SIGNAL, task.getKbUuid());
            }
            Long remaining = stringRedisTemplate.opsForValue().decrement(userIndexKey);
            if (remaining != null && remaining <= 0) {
                stringRedisTemplate.delete(userIndexKey);
            }
        }
    }

    /**
     * 路由到执行器。返回 true = 因版本过期被跳过（已重入队或标记 NONE）
     */
    private boolean route(IndexTask task) {
        if (TARGET_DOCUMENT.equals(task.getTargetType())) {
            return DOC_INDEX_TYPE_EMBEDDING.equals(task.getTaskType())
                    ? executeDocumentEmbedding(task)
                    : executeDocumentGraphical(task);
        }
        return DOC_INDEX_TYPE_EMBEDDING.equals(task.getTaskType())
                ? executeSegmentEmbedding(task)
                : executeSegmentGraphical(task);
    }

    private boolean executeDocumentEmbedding(IndexTask task) {
        KbDocument doc = kbDocumentMapper.getByUuid(task.getDocUuid());
        if (doc == null) {
            return false;
        }
        KnowledgeBase kb = knowledgeBaseMapper.selectOne(new com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper<KnowledgeBase>()
                .eq(KnowledgeBase::getUuid, doc.getKbUuid()).eq(KnowledgeBase::getIsDeleted, false));
        if (kb == null) {
            return false;
        }
        // 开始前版本检查：过期则直接重入队最新版本
        if (versionAdvanced(doc.getIndexVersion(), task.getVersion())) {
            reEnqueueDocument(task, doc);
            return true;
        }
        ChainWrappers.lambdaUpdateChain(kbDocumentMapper)
                .eq(KbDocument::getId, doc.getId())
                .set(KbDocument::getEmbeddingStatusChangeTime, java.time.LocalDateTime.now())
                .set(KbDocument::getEmbeddingStatus, EmbeddingStatusEnum.DOING)
                .update();
        segmentIndexService.reindexEmbedding(kb, doc, versionGuard(task));
        // 条件置位：版本一致才生效，否则说明执行期间内容变更 -> 重入队最新版本
        boolean finalized = ChainWrappers.lambdaUpdateChain(kbDocumentMapper)
                .eq(KbDocument::getId, doc.getId())
                .eq(KbDocument::getIndexVersion, task.getVersion())
                .set(KbDocument::getEmbeddingStatus, EmbeddingStatusEnum.DONE)
                .update();
        if (!finalized) {
            reEnqueueDocument(task, kbDocumentMapper.getByUuid(task.getDocUuid()));
            return true;
        }
        return false;
    }

    private boolean executeDocumentGraphical(IndexTask task) {
        KbDocument doc = kbDocumentMapper.getByUuid(task.getDocUuid());
        if (doc == null) {
            return false;
        }
        KnowledgeBase kb = knowledgeBaseMapper.selectOne(new com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper<KnowledgeBase>()
                .eq(KnowledgeBase::getUuid, doc.getKbUuid()).eq(KnowledgeBase::getIsDeleted, false));
        if (kb == null) {
            return false;
        }
        if (versionAdvanced(doc.getIndexVersion(), task.getVersion())) {
            markDocGraphicalPending(doc.getId());
            return true;
        }
        User user = userService.getById(task.getUserId());
        ChainWrappers.lambdaUpdateChain(kbDocumentMapper)
                .eq(KbDocument::getId, doc.getId())
                .set(KbDocument::getGraphicalStatusChangeTime, java.time.LocalDateTime.now())
                .set(KbDocument::getGraphicalStatus, GraphicalStatusEnum.DOING)
                .update();
        AbstractLLMService llmService = LLMContext.getServiceById(kb.getIngestModelId(), true);
        ChatModel chatModel = llmService.buildChatLLM(
                ChatModelBuilderProperties.builder().temperature(kb.getQueryLlmTemperature()).build());
        // 先清后抽：按账本清理该文档图谱足迹（幂等）；停用段不参与重抽
        knowledgeBaseGraphService.removeDocumentGraphFootprint(kb.getUuid(), doc.getUuid());
        List<DocumentSegment> segments = segmentIndexService.ensureSegments(kb, doc).stream()
                .filter(segment -> !Boolean.FALSE.equals(segment.getIsEnabled()))
                .toList();
        GraphRagContext.get(AdiConstant.RetrieveContentFrom.KNOWLEDGE_BASE).ingest(
                GraphIngestParam.builder()
                        .user(user)
                        .segments(segments)
                        .ChatModel(chatModel)
                        .identifyColumns(List.of(AdiConstant.MetadataKey.KB_UUID))
                        .appendColumns(List.of(AdiConstant.MetadataKey.KB_ITEM_UUID))
                        .isFreeToken(llmService.getAiModel().getIsFree())
                        .sourceId(doc.getId())
                        .modelPlatform(llmService.getAiModel().getPlatform())
                        .modelName(llmService.getAiModel().getName())
                        .build());
        boolean finalized = ChainWrappers.lambdaUpdateChain(kbDocumentMapper)
                .eq(KbDocument::getId, doc.getId())
                .eq(KbDocument::getIndexVersion, task.getVersion())
                .set(KbDocument::getGraphicalStatus, GraphicalStatusEnum.DONE)
                .update();
        if (!finalized) {
            // 执行期间内容变更：图谱数据已过期，标记待重建（图谱手动重跑，不自动重入队）
            markDocGraphicalPending(doc.getId());
            return true;
        }
        return false;
    }

    private boolean executeSegmentEmbedding(IndexTask task) {
        DocumentSegment segment = documentSegmentService.lambdaQuery()
                .eq(DocumentSegment::getUuid, task.getSegmentUuid())
                .eq(DocumentSegment::getIsDeleted, false)
                .one();
        if (segment == null) {
            return false;
        }
        if (versionAdvanced(segment.getIndexVersion(), task.getVersion())) {
            reEnqueueSegment(task, segment);
            return true;
        }
        KbDocument doc = kbDocumentMapper.getByUuid(segment.getDocUuid());
        KnowledgeBase kb = knowledgeBaseMapper.selectOne(new com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper<KnowledgeBase>()
                .eq(KnowledgeBase::getUuid, segment.getKbUuid()).eq(KnowledgeBase::getIsDeleted, false));
        if (doc == null || kb == null) {
            return false;
        }
        segmentIndexService.vectorizeSegment(kb, doc, segment);
        boolean finalized = updateSegmentStatusConditionally(segment.getId(), task.getVersion(),
                DocumentSegment::getEmbeddingStatus, EmbeddingStatusEnum.DONE);
        if (!finalized) {
            reEnqueueSegment(task, documentSegmentService.getById(segment.getId()));
            return true;
        }
        return false;
    }

    private boolean executeSegmentGraphical(IndexTask task) {
        DocumentSegment segment = documentSegmentService.lambdaQuery()
                .eq(DocumentSegment::getUuid, task.getSegmentUuid())
                .eq(DocumentSegment::getIsDeleted, false)
                .one();
        if (segment == null) {
            return false;
        }
        if (versionAdvanced(segment.getIndexVersion(), task.getVersion())) {
            updateSegmentStatus(segment.getId(), DocumentSegment::getGraphicalStatus, GraphicalStatusEnum.NONE);
            return true;
        }
        KbDocument doc = kbDocumentMapper.getByUuid(segment.getDocUuid());
        KnowledgeBase kb = knowledgeBaseMapper.selectOne(new com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper<KnowledgeBase>()
                .eq(KnowledgeBase::getUuid, segment.getKbUuid()).eq(KnowledgeBase::getIsDeleted, false));
        if (doc == null || kb == null) {
            return false;
        }
        User user = userService.getById(task.getUserId());
        // 先幂等清理该段残留（防重复追加）再单段重抽，账本随 ingest 双写重建
        knowledgeBaseGraphService.removeSegmentGraphFootprint(kb.getUuid(), segment.getUuid());
        AbstractLLMService llmService = LLMContext.getServiceById(kb.getIngestModelId(), true);
        ChatModel chatModel = llmService.buildChatLLM(
                ChatModelBuilderProperties.builder().temperature(kb.getQueryLlmTemperature()).build());
        GraphRagContext.get(AdiConstant.RetrieveContentFrom.KNOWLEDGE_BASE).ingest(
                GraphIngestParam.builder()
                        .user(user)
                        .segments(List.of(segment))
                        .ChatModel(chatModel)
                        .identifyColumns(List.of(AdiConstant.MetadataKey.KB_UUID))
                        .appendColumns(List.of(AdiConstant.MetadataKey.KB_ITEM_UUID))
                        .isFreeToken(llmService.getAiModel().getIsFree())
                        .sourceId(doc.getId())
                        .modelPlatform(llmService.getAiModel().getPlatform())
                        .modelName(llmService.getAiModel().getName())
                        .build());
        boolean finalized = updateSegmentStatusConditionally(segment.getId(), task.getVersion(),
                DocumentSegment::getGraphicalStatus, GraphicalStatusEnum.DONE);
        if (!finalized) {
            updateSegmentStatus(segment.getId(), DocumentSegment::getGraphicalStatus, GraphicalStatusEnum.NONE);
            return true;
        }
        return false;
    }

    /**
     * 协作式取消的善后：本任务已写入的部分与历史残留由自己清理
     * （保存侧在检测到 running 时已把清理责任移交到这里），随后重入队最新版本
     */
    private void onCancelled(IndexTask task) {
        try {
            KbDocument doc = kbDocumentMapper.getByUuid(task.getDocUuid());
            if (doc == null) {
                return;
            }
            if (TARGET_SEGMENT.equals(task.getTargetType())) {
                DocumentSegment segment = documentSegmentService.lambdaQuery()
                        .eq(DocumentSegment::getUuid, task.getSegmentUuid())
                        .eq(DocumentSegment::getIsDeleted, false)
                        .one();
                if (segment != null) {
                    updateSegmentStatus(segment.getId(), DocumentSegment::getEmbeddingStatus, EmbeddingStatusEnum.NONE);
                }
                return;
            }
            if (DOC_INDEX_TYPE_EMBEDDING.equals(task.getTaskType())) {
                iKnowledgeEmbeddingService.deleteByItemUuid(task.getDocUuid());
                documentSegmentService.deleteByDocUuid(task.getDocUuid());
                ChainWrappers.lambdaUpdateChain(kbDocumentMapper)
                        .eq(KbDocument::getId, doc.getId())
                        .set(KbDocument::getEmbeddingStatus, EmbeddingStatusEnum.NONE)
                        .update();
                reEnqueueDocument(task, doc);
            }
        } catch (Exception e) {
            log.error("onCancelled cleanup error, docUuid:{}", task.getDocUuid(), e);
        }
    }

    private Supplier<Boolean> versionGuard(IndexTask task) {
        return () -> {
            KbDocument doc = kbDocumentMapper.getByUuid(task.getDocUuid());
            return doc == null || versionAdvanced(doc.getIndexVersion(), task.getVersion());
        };
    }

    private void reEnqueueDocument(IndexTask task, KbDocument doc) {
        if (doc == null) {
            return;
        }
        IndexTask next = new IndexTask();
        next.setKbUuid(task.getKbUuid());
        next.setDocUuid(task.getDocUuid());
        next.setUserId(task.getUserId());
        next.setSegmentUuid("");
        next.setTargetType(TARGET_DOCUMENT);
        next.setTaskType(task.getTaskType());
        next.setVersion(doc.getIndexVersion() == null ? 0 : doc.getIndexVersion());
        indexTaskMapper.enqueue(next);
    }

    private void reEnqueueSegment(IndexTask task, DocumentSegment segment) {
        if (segment == null) {
            return;
        }
        IndexTask next = new IndexTask();
        next.setKbUuid(task.getKbUuid());
        next.setDocUuid(task.getDocUuid());
        next.setUserId(task.getUserId());
        next.setSegmentUuid(task.getSegmentUuid());
        next.setTargetType(TARGET_SEGMENT);
        next.setTaskType(task.getTaskType());
        next.setVersion(segment.getIndexVersion() == null ? 0 : segment.getIndexVersion());
        indexTaskMapper.enqueue(next);
    }

    private boolean updateSegmentStatusConditionally(Long segmentId, int expectedVersion,
                                                     SFunction<DocumentSegment, ?> column, Object status) {
        return ChainWrappers.lambdaUpdateChain(documentSegmentService.getBaseMapper())
                .eq(DocumentSegment::getId, segmentId)
                .eq(DocumentSegment::getIndexVersion, expectedVersion)
                .set(column, status)
                .update();
    }

    private void updateSegmentStatus(Long segmentId, SFunction<DocumentSegment, ?> column, Object status) {
        ChainWrappers.lambdaUpdateChain(documentSegmentService.getBaseMapper())
                .eq(DocumentSegment::getId, segmentId)
                .set(column, status)
                .update();
    }

    private void markDocGraphicalPending(Long docId) {
        ChainWrappers.lambdaUpdateChain(kbDocumentMapper)
                .eq(KbDocument::getId, docId)
                .set(KbDocument::getGraphicalStatus, GraphicalStatusEnum.NONE)
                .update();
    }

    private boolean versionAdvanced(Integer current, int snapshot) {
        return current != null && !Objects.equals(current, snapshot);
    }
}

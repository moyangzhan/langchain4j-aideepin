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
import jakarta.annotation.PreDestroy;
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
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Supplier;

import static com.moyz.adi.common.cosntant.AdiConstant.DOC_INDEX_TYPE_EMBEDDING;
import static com.moyz.adi.common.cosntant.AdiConstant.DOC_INDEX_TYPE_GRAPHICAL;
import static com.moyz.adi.common.cosntant.RedisKeyConstant.KB_STATISTIC_RECALCULATE_SIGNAL;
import static com.moyz.adi.common.cosntant.RedisKeyConstant.USER_INDEXING;

/**
 * 索引任务队列：一切索引写入（切段/向量化/图谱抽取）的唯一通道。
 * <p>
 * 总原则：同 doc 任务串行（claimOne 领取互斥，advisory lock），跨 doc 并行。
 * 版本入键——(doc, segment, target, type, index_version) 唯一（部分索引，done 行除外）：
 * 新版本 enqueue 永远插新行（running 行吞不掉它），并 supersede 同键旧版本行
 * （pending 直接置 failed；running 仅打 stop_flag，由检查点协作中止，串行闸门不提前
 * 打开）；同版本重复触发合并去抖、failed 原地复活（手动重试）。
 * 执行器三道过期防线（开始检查 / 批间协作检查点 / 结束条件置位）：发现版本前进即
 * 作废自身并接管"变更即失效"的清理（被取代方删旧段行与向量，最新版本任务必走全量
 * 重切），同时幂等补入队最新版本作安全网（兜触发方 bump 与 enqueue 之间崩溃的缺口）。
 * 图谱不自动触发（仅保存与段启用自动入队 embedding），失败仅手动重试。
 * 崩溃自愈：执行器周期刷新 heartbeat_time，轮询按其回收超时 running（进程死→重置
 * pending 重跑）；start_time 超过最大执行时长的心跳存活任务视为挂起，强制 failed
 * （兜底断路器，拦"进程活着但永久卡死"）。
 */
@Slf4j
@Service
public class IndexTaskService {

    public static final String TARGET_DOCUMENT = "document";
    public static final String TARGET_SEGMENT = "segment";
    public static final String STATUS_DONE = "done";
    public static final String STATUS_FAILED = "failed";

    /** 心跳间隔：执行器刷新 heartbeat_time 的周期 */
    private static final int HEARTBEAT_INTERVAL_SECONDS = 30;

    /** 心跳超时阈值（约 4 个漏拍即判定进程死亡），running 行重置 pending 重跑 */
    private static final int STALE_RUNNING_MINUTES = 2;

    /** 最大执行时长兜底断路器：进程活着但挂起的任务强制 failed，须远大于任何正常任务 */
    private static final int MAX_RUNNING_MINUTES = 120;

    private final ScheduledExecutorService heartbeatExecutor = Executors.newSingleThreadScheduledExecutor(r -> {
        Thread t = new Thread(r, "index-task-heartbeat");
        t.setDaemon(true);
        return t;
    });

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
     * 文档级任务入队（版本入键：新版本插新行并 supersede 旧版本；同版本合并/复活）
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
        task.setIndexVersion(doc.getIndexVersion() == null ? 0 : doc.getIndexVersion());
        indexTaskMapper.supersede(task);
        indexTaskMapper.enqueue(task);
        self.dispatch();
    }

    /**
     * 段级任务入队（目标段必须存在；supersede+upsert 语义同文档级）
     */
    public void enqueueSegment(KnowledgeBase kb, KbDocument doc, DocumentSegment segment, String taskType, User user) {
        IndexTask task = new IndexTask();
        task.setKbUuid(kb.getUuid());
        task.setDocUuid(doc.getUuid());
        task.setUserId(user.getId());
        task.setSegmentUuid(segment.getUuid());
        task.setTargetType(TARGET_SEGMENT);
        task.setTaskType(taskType);
        task.setIndexVersion(segment.getIndexVersion() == null ? 0 : segment.getIndexVersion());
        indexTaskMapper.supersede(task);
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

    @PreDestroy
    public void shutdownHeartbeatExecutor() {
        heartbeatExecutor.shutdownNow();
    }

    @Transactional
    public IndexTask claimOne() {
        return indexTaskMapper.claimOne();
    }

    /**
     * 轮询兜底：按 heartbeat_time 回收超时 running（进程崩溃遗留，重置 pending 重跑），
     * 按 start_time 熔断挂起任务（进程活着但卡死，强制 failed）。先回收后熔断——
     * 两者同时命中时按崩溃处理（自动重跑优于人工重试）。有动作则重触发消费。
     */
    @Scheduled(fixedDelay = 60_000)
    public void pollStale() {
        try {
            int reset = indexTaskMapper.resetStaleRunning(STALE_RUNNING_MINUTES);
            int failed = indexTaskMapper.failOverdue(MAX_RUNNING_MINUTES);
            if (reset + failed > 0) {
                log.warn("Poller recovered index tasks: {} stale-running reset to pending (> {} min), {} hung force-failed (> {} min)",
                        reset, STALE_RUNNING_MINUTES, failed, MAX_RUNNING_MINUTES);
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
        ScheduledFuture<?> heartbeat = startHeartbeat(task);
        try {
            boolean skipped = route(task);
            indexTaskMapper.finishOne(task.getId(), STATUS_DONE, skipped ? "skipped: superseded by newer version" : null);
        } catch (IndexTaskCancelledException e) {
            log.info("Index task cancelled, docUuid:{}, reason:{}", task.getDocUuid(), e.getMessage());
            onCancelled(task);
            indexTaskMapper.finishOne(task.getId(), STATUS_DONE, "skipped: superseded by newer version");
        } catch (Exception e) {
            log.error("Index task failed, docUuid:{}, targetType:{}, taskType:{}",
                    task.getDocUuid(), task.getTargetType(), task.getTaskType(), e);
            indexTaskMapper.finishOne(task.getId(), STATUS_FAILED, StringUtils.abbreviate(e.getMessage(), 500));
        } finally {
            heartbeat.cancel(false);
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
     * 执行期心跳：向本任务行周期刷新 heartbeat_time。刷新失效（rowcount=0，行已被
     * stale 重置或超时强杀）说明当前执行器已沦为僵尸，告警一次留痕。
     */
    private ScheduledFuture<?> startHeartbeat(IndexTask task) {
        AtomicBoolean zombieReported = new AtomicBoolean(false);
        return heartbeatExecutor.scheduleAtFixedRate(() -> {
            try {
                if (indexTaskMapper.heartbeat(task.getId()) == 0
                        && zombieReported.compareAndSet(false, true)) {
                    log.warn("Index task heartbeat lost: row no longer running (reset or force-failed), id:{}, docUuid:{}",
                            task.getId(), task.getDocUuid());
                }
            } catch (Exception e) {
                log.warn("Index task heartbeat error, id:{}", task.getId(), e);
            }
        }, HEARTBEAT_INTERVAL_SECONDS, HEARTBEAT_INTERVAL_SECONDS, TimeUnit.SECONDS);
    }

    /**
     * 路由到执行器。返回 true = 因版本过期被跳过（embedding 已接管清理并幂等补入队
     * 最新版本；graphical 仅标记待重建，手动重跑，不自动重入队）
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
        // 开始前版本检查：过期则接管保存侧移交的清理（删段行向量，最新版本任务走全量重切）
        if (versionAdvanced(doc.getIndexVersion(), task.getIndexVersion())) {
            invalidateDocIndexArtifacts(doc);
            enqueueLatestDocument(task, doc);
            return true;
        }
        ChainWrappers.lambdaUpdateChain(kbDocumentMapper)
                .eq(KbDocument::getId, doc.getId())
                .set(KbDocument::getEmbeddingStatusChangeTime, java.time.LocalDateTime.now())
                .set(KbDocument::getEmbeddingStatus, EmbeddingStatusEnum.DOING)
                .update();
        segmentIndexService.reindexEmbedding(kb, doc, versionGuard(task));
        // 条件置位：版本一致才生效，否则说明执行期间版本前进 ->
        // 本次产出全部过期，同样接管清理（漏检取消时段行仍在，不删会让最新版本任务走增量分支嵌旧内容）
        boolean finalized = ChainWrappers.lambdaUpdateChain(kbDocumentMapper)
                .eq(KbDocument::getId, doc.getId())
                .eq(KbDocument::getIndexVersion, task.getIndexVersion())
                .set(KbDocument::getEmbeddingStatus, EmbeddingStatusEnum.DONE)
                .update();
        if (!finalized) {
            invalidateDocIndexArtifacts(doc);
            enqueueLatestDocument(task, kbDocumentMapper.getByUuid(task.getDocUuid()));
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
        if (versionAdvanced(doc.getIndexVersion(), task.getIndexVersion())) {
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
                .eq(KbDocument::getIndexVersion, task.getIndexVersion())
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
        // 停用段跳过（编辑/新增问题/子块对停用段照常入队，统一由这里跳过，不复活停用数据）
        if (Boolean.FALSE.equals(segment.getIsEnabled())) {
            return false;
        }
        // 过期即作废自身：清 embedding_id 保证最新版本任务重嵌（否则本任务迟到写入的
        // 旧向量 id 会让新任务误判"已嵌"而跳过），并幂等补入队最新版本
        if (versionAdvanced(segment.getIndexVersion(), task.getIndexVersion())) {
            invalidateSegmentEmbedding(segment);
            enqueueLatestSegment(task, segment);
            return true;
        }
        KbDocument doc = kbDocumentMapper.getByUuid(segment.getDocUuid());
        KnowledgeBase kb = knowledgeBaseMapper.selectOne(new com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper<KnowledgeBase>()
                .eq(KnowledgeBase::getUuid, segment.getKbUuid()).eq(KnowledgeBase::getIsDeleted, false));
        if (doc == null || kb == null) {
            return false;
        }
        segmentIndexService.vectorizeSegment(kb, doc, segment);
        boolean finalized = updateSegmentStatusConditionally(segment.getId(), task.getIndexVersion(),
                DocumentSegment::getEmbeddingStatus, EmbeddingStatusEnum.DONE);
        if (!finalized) {
            // 执行期间段内容变更：本次向量已过期，同样清引用并补最新版本任务
            DocumentSegment fresh = documentSegmentService.getById(segment.getId());
            invalidateSegmentEmbedding(segment);
            enqueueLatestSegment(task, fresh);
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
        // 停用段跳过（编辑/新增问题/子块对停用段照常入队，统一由这里跳过，不复活停用数据）
        if (Boolean.FALSE.equals(segment.getIsEnabled())) {
            return false;
        }
        if (versionAdvanced(segment.getIndexVersion(), task.getIndexVersion())) {
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
        boolean finalized = updateSegmentStatusConditionally(segment.getId(), task.getIndexVersion(),
                DocumentSegment::getGraphicalStatus, GraphicalStatusEnum.DONE);
        if (!finalized) {
            updateSegmentStatus(segment.getId(), DocumentSegment::getGraphicalStatus, GraphicalStatusEnum.NONE);
            return true;
        }
        return false;
    }

    /**
     * 协作式取消的善后：本任务已写入的部分与历史残留由自己清理
     * （保存侧在检测到 running 时已把清理责任移交到这里），随后幂等补入队最新版本。
     * 版本入键后 enqueue 不会被自身 running 行吞掉（取消仅因版本前进触发，
     * 最新版本的键必然与当前行不同）。
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
                    invalidateSegmentEmbedding(segment);
                }
                return;
            }
            if (DOC_INDEX_TYPE_EMBEDDING.equals(task.getTaskType())) {
                invalidateDocIndexArtifacts(doc);
                enqueueLatestDocument(task, doc);
            }
        } catch (Exception e) {
            log.error("onCancelled cleanup error, docUuid:{}", task.getDocUuid(), e);
        }
    }

    /**
     * 协作式取消信号：stop_flag（新版本入队对本行打的停止标志，任务行本地信号）或
     * 版本前进任一成立即作废
     */
    private Supplier<Boolean> versionGuard(IndexTask task) {
        return () -> {
            if (indexTaskMapper.isStopFlagSet(task.getId())) {
                return true;
            }
            KbDocument doc = kbDocumentMapper.getByUuid(task.getDocUuid());
            return doc == null || versionAdvanced(doc.getIndexVersion(), task.getIndexVersion());
        };
    }

    /**
     * 幂等安全网入队最新版本：正常情况下新版本行已由触发方入队（版本入键，enqueue
     * 不会被 running 行吞掉），此处兜"版本已推进但触发方在 bump 与 enqueue 之间崩溃"
     * 的缺口——同键同版本 upsert，已有 pending 则合并空转。
     */
    private void enqueueLatestDocument(IndexTask task, KbDocument doc) {
        if (doc == null) {
            return;
        }
        indexTaskMapper.enqueue(buildLatestDocumentTask(task, doc));
    }

    private void enqueueLatestSegment(IndexTask task, DocumentSegment segment) {
        if (segment == null) {
            return;
        }
        indexTaskMapper.enqueue(buildLatestSegmentTask(task, segment));
    }

    /**
     * 文档级 embedding 作废时的善后清理（保存侧在 running 期间移交的责任）：
     * 版本前进即段行/向量全部过期（变更即失效），删除后最新版本任务走全量重切分支重建。
     * 供开始检查/结束置位失败/协作取消三个检测点复用。
     */
    private void invalidateDocIndexArtifacts(KbDocument doc) {
        iKnowledgeEmbeddingService.deleteByItemUuid(doc.getUuid());
        documentSegmentService.deleteByDocUuid(doc.getUuid());
        ChainWrappers.lambdaUpdateChain(kbDocumentMapper)
                .eq(KbDocument::getId, doc.getId())
                .set(KbDocument::getEmbeddingStatus, EmbeddingStatusEnum.NONE)
                .update();
    }

    /**
     * 段级 embedding 过期善后：清 embedding_id 与状态，保证最新版本段任务重嵌
     * （否则本任务迟到写入的旧向量 id 会让新任务误判"已嵌"而跳过）
     */
    private void invalidateSegmentEmbedding(DocumentSegment segment) {
        ChainWrappers.lambdaUpdateChain(documentSegmentService.getBaseMapper())
                .eq(DocumentSegment::getId, segment.getId())
                .set(DocumentSegment::getEmbeddingId, null)
                .set(DocumentSegment::getEmbeddingStatus, EmbeddingStatusEnum.NONE)
                .update();
    }

    /**
     * 构造最新版本的文档级任务（安全网/取消善后用，直接 enqueue）
     */
    private IndexTask buildLatestDocumentTask(IndexTask task, KbDocument doc) {
        IndexTask next = new IndexTask();
        next.setKbUuid(task.getKbUuid());
        next.setDocUuid(task.getDocUuid());
        next.setUserId(task.getUserId());
        next.setSegmentUuid("");
        next.setTargetType(TARGET_DOCUMENT);
        next.setTaskType(task.getTaskType());
        next.setIndexVersion(doc.getIndexVersion() == null ? 0 : doc.getIndexVersion());
        return next;
    }

    /**
     * 构造最新版本的段级任务（安全网/取消善后用，直接 enqueue）
     */
    private IndexTask buildLatestSegmentTask(IndexTask task, DocumentSegment segment) {
        IndexTask next = new IndexTask();
        next.setKbUuid(task.getKbUuid());
        next.setDocUuid(task.getDocUuid());
        next.setUserId(task.getUserId());
        next.setSegmentUuid(task.getSegmentUuid());
        next.setTargetType(TARGET_SEGMENT);
        next.setTaskType(task.getTaskType());
        next.setIndexVersion(segment.getIndexVersion() == null ? 0 : segment.getIndexVersion());
        return next;
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

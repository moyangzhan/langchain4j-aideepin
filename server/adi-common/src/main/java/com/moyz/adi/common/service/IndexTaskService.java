package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.core.toolkit.support.SFunction;
import com.baomidou.mybatisplus.extension.toolkit.ChainWrappers;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.dto.IndexFailureDto;
import com.moyz.adi.common.entity.DocumentSegment;
import com.moyz.adi.common.entity.IndexTask;
import com.moyz.adi.common.entity.KbDocument;
import com.moyz.adi.common.entity.KnowledgeBase;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.enums.EmbeddingStatusEnum;
import com.moyz.adi.common.enums.GraphicalStatusEnum;
import com.moyz.adi.common.util.AdiStringUtil;
import com.moyz.adi.common.enums.SegmentModeEnum;
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
import java.util.stream.Collectors;

import static com.moyz.adi.common.cosntant.AdiConstant.DOC_INDEX_TYPE_EMBEDDING;
import static com.moyz.adi.common.cosntant.AdiConstant.DOC_INDEX_TYPE_GRAPHICAL;
import static com.moyz.adi.common.cosntant.RedisKeyConstant.KB_STATISTIC_RECALCULATE_SIGNAL;
import static com.moyz.adi.common.cosntant.RedisKeyConstant.USER_INDEXING;

/**
 * Index task queue: the single channel for all index writes (segmentation / embedding / graph extraction).
 * <p>
 * Tasks on the same doc run serially (claimOne mutual exclusion via advisory lock), across docs
 * in parallel. The uniqueness key is (doc, segment, target, type, index_version): a newer version
 * enqueues a new row and supersedes same-key rows of older versions (pending → failed; running →
 * stop_flag, aborted cooperatively at the next checkpoint); the same version merges into the
 * existing pending row; failed rows revive in place on manual retry.
 * Executors check staleness at three checkpoints (start check / per-batch checkpoint / conditional
 * finalize): on version advance they discard their own output, clean up stale segment rows and
 * vectors, and idempotently re-enqueue the latest version.
 * Embedding tasks are enqueued automatically (on save and on segment enable); graph tasks and
 * retries of failed tasks are enqueued manually.
 * Executors refresh heartbeat_time periodically; the poller resets timed-out running rows to
 * pending and force-fails tasks exceeding the max runtime.
 */
@Slf4j
@Service
public class IndexTaskService {

    public static final String TARGET_DOCUMENT = "document";
    public static final String TARGET_SEGMENT = "segment";
    public static final String STATUS_DONE = "done";
    public static final String STATUS_FAILED = "failed";

    /** Heartbeat interval: how often an executor refreshes heartbeat_time */
    private static final int HEARTBEAT_INTERVAL_SECONDS = 30;

    /** Heartbeat timeout (about 4 missed beats means a dead process); running rows are reset to pending for rerun */
    private static final int STALE_RUNNING_MINUTES = 2;

    /** Max-runtime circuit breaker: force-fails hung tasks in a live process; must far exceed any normal task */
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
    private ModelHealthService modelHealthService;

    // @Lazy breaks the bean cycle; userService is only accessed when a task executes
    @Lazy
    @Resource
    private UserService userService;

    @Resource
    private StringRedisTemplate stringRedisTemplate;

    /**
     * Enqueue a document-level task (version in key: a newer version inserts a new row and
     * supersedes older ones; the same version merges or revives)
     */
    public void enqueueDocument(String kbUuid, String docUuid, String taskType, User user) {
        KbDocument doc = kbDocumentMapper.getByUuid(docUuid);
        if (doc == null) {
            log.warn("enqueueDocument skipped, doc not found:{}", docUuid);
            return;
        }
        // never (re)index a soft-deleted document: a stale trigger must not resurrect it
        if (Boolean.TRUE.equals(doc.getIsDeleted())) {
            log.warn("enqueueDocument skipped, doc deleted:{}", docUuid);
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
        // an empty qa doc has nothing to index and keeps its current status
        boolean emptyQaDoc = SegmentModeEnum.QA == doc.getSegmentMode()
                && documentSegmentService.listByDocUuid(doc.getUuid()).isEmpty();
        if (!emptyQaDoc) {
            boolean embedding = DOC_INDEX_TYPE_EMBEDDING.equals(taskType);
            ChainWrappers.lambdaUpdateChain(kbDocumentMapper)
                    .eq(KbDocument::getId, doc.getId())
                    .set(embedding, KbDocument::getEmbeddingStatus, EmbeddingStatusEnum.DOING)
                    .set(embedding, KbDocument::getEmbeddingStatusChangeTime, java.time.LocalDateTime.now())
                    .set(!embedding, KbDocument::getGraphicalStatus, GraphicalStatusEnum.DOING)
                    .set(!embedding, KbDocument::getGraphicalStatusChangeTime, java.time.LocalDateTime.now())
                    .update();
        }
        self.dispatch();
    }

    /**
     * Enqueue a segment-level task (the target segment must exist; supersede + upsert
     * semantics identical to the document level)
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
     * Whether the doc currently has a running task (the save side decides: clean up now
     * or hand the cleanup over)
     */
    public boolean hasRunningByDoc(String docUuid) {
        return indexTaskMapper.hasRunningByDoc(docUuid);
    }

    /**
     * Whether the doc has a queued or executing task: retry enqueues without touching the doc
     * status, so the queue wait still reads FAIL on the doc — the frontend distinguishes
     * "queued/executing" from "finally failed" with this.
     */
    public boolean hasUnfinishedByDoc(String docUuid) {
        return indexTaskMapper.hasUnfinishedByDoc(docUuid);
    }

    /**
     * Latest failure per index dimension (embedding/graphical) for the detail page failure
     * list; the doc row keeps only one fail_reason, so simultaneous failures need the task
     * table. Ties on update_time are deduplicated by task type.
     */
    public List<IndexFailureDto> listDocumentFailures(String docUuid) {
        return indexTaskMapper.listLatestFailedByDoc(docUuid).stream()
                .collect(Collectors.toMap(IndexTask::getTaskType,
                        t -> IndexFailureDto.builder()
                                .taskType(t.getTaskType())
                                .failReason(t.getFailReason())
                                .updateTime(t.getUpdateTime())
                                .build(),
                        (a, b) -> a))
                .values()
                .stream()
                .toList();
    }

    /**
     * Cancel every unfinished task of a document (called on document delete): pending rows
     * fail in place, running rows get stop_flag and abort at their next checkpoint.
     */
    public void cancelByDoc(String docUuid) {
        indexTaskMapper.cancelUnfinishedByDoc(docUuid, "document deleted");
    }

    /**
     * Consume loop: claim and execute until nothing is claimable. Triggered on enqueue;
     * re-triggered by the poller as a fallback.
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
     * Polling fallback: recover timed-out running rows by heartbeat_time (left over by a
     * crashed process; reset to pending for rerun) and force-fail hung tasks by start_time
     * (process alive but stuck). Recover first, force-fail second — when both hit, treat as
     * crash (auto rerun over manual retry). Re-trigger consumption if anything was done.
     */
    @Scheduled(fixedDelay = 60_000)
    public void pollStale() {
        try {
            int reset = indexTaskMapper.resetStaleRunning(STALE_RUNNING_MINUTES);
            List<IndexTask> broken = indexTaskMapper.failOverdue(MAX_RUNNING_MINUTES);
            for (IndexTask task : broken) {
                markHostFailed(task, "max execution time exceeded");
            }
            if (reset + broken.size() > 0) {
                log.warn("Poller recovered index tasks: {} stale-running reset to pending (> {} min), {} hung force-failed (> {} min)",
                        reset, STALE_RUNNING_MINUTES, broken.size(), MAX_RUNNING_MINUTES);
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
            indexTaskMapper.finishOne(task.getId(), STATUS_DONE, skipped ? "skipped: superseded by newer version" : "");
        } catch (IndexTaskCancelledException e) {
            log.info("Index task cancelled, docUuid:{}, reason:{}", task.getDocUuid(), e.getMessage());
            onCancelled(task);
            indexTaskMapper.finishOne(task.getId(), STATUS_DONE, "skipped: superseded by newer version");
        } catch (Exception e) {
            log.error("Index task failed, docUuid:{}, targetType:{}, taskType:{}",
                    task.getDocUuid(), task.getTargetType(), task.getTaskType(), e);
            // Sanitize the failure reason: providers often return the whole JSON body as the
            // exception message, which is unreadable in fail_reason
            String reason = AdiStringUtil.extractJsonMessage(
                    e.getMessage() != null ? e.getMessage() : e.getClass().getSimpleName());
            // finishOne returns 0 when the row was already taken over (stale-reset or force-failed):
            // a zombie must not finalize the host row either — that belongs to the rerun/breaker
            if (indexTaskMapper.finishOne(task.getId(), STATUS_FAILED, StringUtils.abbreviate(reason, 500)) > 0) {
                markHostFailed(task, reason);
            }
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
     * Runtime heartbeat: periodically refresh heartbeat_time on this task row. A failed
     * refresh (rowcount=0, row already stale-reset or force-failed) means this executor
     * has become a zombie; warn once for the record.
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
     * Route to an executor. Returns true = skipped as stale (embedding already took over
     * cleanup and idempotently re-enqueued the latest version; graphical only marks pending
     * rebuild for a manual rerun, no auto re-enqueue)
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
        if (doc == null || Boolean.TRUE.equals(doc.getIsDeleted())) {
            return false;
        }
        KnowledgeBase kb = knowledgeBaseMapper.selectOne(new com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper<KnowledgeBase>()
                .eq(KnowledgeBase::getUuid, doc.getKbUuid()).eq(KnowledgeBase::getIsDeleted, false));
        if (kb == null) {
            return false;
        }
        // Pre-start version check: if stale, take over the cleanup handed over by the save
        // side (delete segment rows and vectors; the latest-version task does a full re-segmentation)
        if (versionAdvanced(doc.getIndexVersion(), task.getIndexVersion())) {
            invalidateDocIndexArtifacts(doc);
            enqueueLatestDocument(task, doc);
            return true;
        }
        // A QA-mode doc with no segment rows has no QA data yet (generation/import pending):
        // finalization belongs to whichever flow fills the data — bail out BEFORE marking DOING,
        // otherwise the doc stays DOING forever with nothing to finalize it
        if (SegmentModeEnum.QA == doc.getSegmentMode()
                && documentSegmentService.listByDocUuid(doc.getUuid()).isEmpty()) {
            return false;
        }
        ChainWrappers.lambdaUpdateChain(kbDocumentMapper)
                .eq(KbDocument::getId, doc.getId())
                .set(KbDocument::getEmbeddingStatusChangeTime, java.time.LocalDateTime.now())
                .set(KbDocument::getEmbeddingStatus, EmbeddingStatusEnum.DOING)
                .set(KbDocument::getFailReason, "")
                .update();
        segmentIndexService.reindexEmbedding(kb, doc, versionGuard(task));
        // Conditional finalize: only takes effect when the version still matches; otherwise the
        // version advanced during execution -> this run's output is entirely stale, so take over
        // the cleanup too (segments written before a missed cancellation are still there, and
        // leaving them would send the latest-version task down the incremental branch with stale content)
        boolean finalized = ChainWrappers.lambdaUpdateChain(kbDocumentMapper)
                .eq(KbDocument::getId, doc.getId())
                .eq(KbDocument::getIndexVersion, task.getIndexVersion())
                .set(KbDocument::getEmbeddingStatus, EmbeddingStatusEnum.DONE)
                .set(KbDocument::getFailReason, "")
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
        if (doc == null || Boolean.TRUE.equals(doc.getIsDeleted())) {
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
                .set(KbDocument::getFailReason, "")
                .update();
        AbstractLLMService llmService = LLMContext.getServiceById(kb.getIngestModelId(), true);
        ChatModel chatModel = llmService.buildChatLLM(
                ChatModelBuilderProperties.builder().temperature(kb.getQueryLlmTemperature()).build());
        // Clear before extracting: remove the doc's graph footprint via the ledger (idempotent);
        // disabled segments are excluded from re-extraction
        knowledgeBaseGraphService.removeDocumentGraphFootprint(kb.getUuid(), doc.getUuid());
        List<DocumentSegment> segments = segmentIndexService.ensureSegments(kb, doc).stream()
                .filter(segment -> !Boolean.FALSE.equals(segment.getIsEnabled()))
                .toList();
        ingestGraph(doc, user, segments, llmService, chatModel);
        boolean finalized = ChainWrappers.lambdaUpdateChain(kbDocumentMapper)
                .eq(KbDocument::getId, doc.getId())
                .eq(KbDocument::getIndexVersion, task.getIndexVersion())
                .set(KbDocument::getGraphicalStatus, GraphicalStatusEnum.DONE)
                .set(KbDocument::getFailReason, "")
                .update();
        if (!finalized) {
            // Content changed during execution: graph data is stale, mark pending rebuild
            // (graph reruns manually, no auto re-enqueue)
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
        // Skip disabled segments (edits/added questions/child chunks still enqueue them; they are
        // skipped here uniformly, never reviving disabled data)
        if (Boolean.FALSE.equals(segment.getIsEnabled())) {
            return false;
        }
        // Stale means self-invalidate: clear embedding_id so the latest-version task re-embeds
        // (otherwise the stale vector id this task writes late makes the new task mistake it as
        // already embedded and skip), and idempotently re-enqueue the latest version
        if (versionAdvanced(segment.getIndexVersion(), task.getIndexVersion())) {
            invalidateSegmentEmbedding(segment);
            enqueueLatestSegment(task, segment);
            return true;
        }
        KbDocument doc = kbDocumentMapper.getByUuid(segment.getDocUuid());
        KnowledgeBase kb = knowledgeBaseMapper.selectOne(new com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper<KnowledgeBase>()
                .eq(KnowledgeBase::getUuid, segment.getKbUuid()).eq(KnowledgeBase::getIsDeleted, false));
        if (doc == null || Boolean.TRUE.equals(doc.getIsDeleted()) || kb == null) {
            return false;
        }
        segmentIndexService.vectorizeSegment(kb, doc, segment);
        boolean finalized = updateSegmentStatusConditionally(segment.getId(), task.getIndexVersion(),
                DocumentSegment::getEmbeddingStatus, EmbeddingStatusEnum.DONE);
        if (!finalized) {
            // Segment content changed during execution: this run's vector is stale; likewise
            // clear the reference and re-enqueue the latest-version task
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
        // Skip disabled segments (edits/added questions/child chunks still enqueue them; they are
        // skipped here uniformly, never reviving disabled data)
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
        if (doc == null || Boolean.TRUE.equals(doc.getIsDeleted()) || kb == null) {
            return false;
        }
        User user = userService.getById(task.getUserId());
        // Idempotently clear this segment's leftovers first (prevents duplicate appends) then
        // re-extract it alone; the ledger is rebuilt by the ingest double-write
        knowledgeBaseGraphService.removeSegmentGraphFootprint(kb.getUuid(), segment.getUuid());
        AbstractLLMService llmService = LLMContext.getServiceById(kb.getIngestModelId(), true);
        ChatModel chatModel = llmService.buildChatLLM(
                ChatModelBuilderProperties.builder().temperature(kb.getQueryLlmTemperature()).build());
        ingestGraph(doc, user, List.of(segment), llmService, chatModel);
        boolean finalized = updateSegmentStatusConditionally(segment.getId(), task.getIndexVersion(),
                DocumentSegment::getGraphicalStatus, GraphicalStatusEnum.DONE);
        if (!finalized) {
            updateSegmentStatus(segment.getId(), DocumentSegment::getGraphicalStatus, GraphicalStatusEnum.NONE);
            return true;
        }
        return false;
    }

    /**
     * Cleanup after cooperative cancellation: whatever this task already wrote plus historical
     * leftovers is cleaned up by itself (the save side handed the cleanup duty here when it saw
     * the task running), then the latest version is idempotently re-enqueued. With version in
     * key, enqueue is never swallowed by this task's own running row (cancellation only fires on
     * version advance, so the latest version's key always differs from the current row).
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
     * Cooperative cancellation signal: invalidate as soon as either stop_flag (stop marker set
     * on this row by a newer-version enqueue; row-local signal) or version advance holds
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
     * Ingest segments into the graph store; failures are recorded against the ingest model's health
     */
    private void ingestGraph(KbDocument doc, User user, List<DocumentSegment> segments,
                             AbstractLLMService llmService, ChatModel chatModel) {
        try {
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
        } catch (Exception e) {
            String modelName = llmService.getAiModel().getName();
            modelHealthService.recordFailure(modelName, e);
            // Rethrow with the sanitized message plus the model actually used (health
            // fallback may differ from the KB's ingest model); it lands in fail_reason
            String friendly = AdiStringUtil.extractJsonMessage(
                    e.getMessage() != null ? e.getMessage() : e.getClass().getSimpleName());
            throw new RuntimeException(friendly + ", name: " + modelName, e);
        }
    }

    /**
     * Idempotent safety-net enqueue of the latest version: normally the trigger side has already
     * enqueued the new-version row (version in key; enqueue is not swallowed by a running row);
     * this covers the gap where the version already advanced but the trigger crashed between
     * bump and enqueue — same key same version upsert, an existing pending row just merges into
     * a no-op.
     */
    private void enqueueLatestDocument(IndexTask task, KbDocument doc) {
        if (doc == null || Boolean.TRUE.equals(doc.getIsDeleted())) {
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
     * Post-invalidation cleanup for document-level embedding (duty handed over by the save side
     * while running): on version advance all segment rows/vectors are stale (invalidate on
     * change); after deletion the latest-version task rebuilds via the full re-segmentation
     * branch. Reused by the three checkpoints: start check, finalize failure, cooperative
     * cancellation.
     */
    private void invalidateDocIndexArtifacts(KbDocument doc) {
        iKnowledgeEmbeddingService.deleteByItemUuid(doc.getUuid());
        documentSegmentService.deleteByDocUuid(doc.getUuid());
        ChainWrappers.lambdaUpdateChain(kbDocumentMapper)
                .eq(KbDocument::getId, doc.getId())
                .set(KbDocument::getEmbeddingStatus, EmbeddingStatusEnum.NONE)
                .set(KbDocument::getFailReason, "")
                .update();
    }

    /**
     * Post-invalidation cleanup for stale segment embedding: clears embedding_id and status so
     * the latest-version segment task re-embeds (otherwise the stale vector id this task writes
     * late makes the new task mistake it as already embedded and skip)
     */
    private void invalidateSegmentEmbedding(DocumentSegment segment) {
        ChainWrappers.lambdaUpdateChain(documentSegmentService.getBaseMapper())
                .eq(DocumentSegment::getId, segment.getId())
                .set(DocumentSegment::getEmbeddingId, null)
                .set(DocumentSegment::getEmbeddingStatus, EmbeddingStatusEnum.NONE)
                .set(DocumentSegment::getFailReason, "")
                .update();
    }

    /**
     * Build the latest-version document-level task (for the safety net / cancellation cleanup;
     * enqueued directly)
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
     * Build the latest-version segment-level task (for the safety net / cancellation cleanup;
     * enqueued directly)
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

    /**
     * Conditional finalize to ready (DONE): only used on success paths, so fail_reason is
     * cleared in the same atomic update
     */
    private boolean updateSegmentStatusConditionally(Long segmentId, int expectedVersion,
                                                     SFunction<DocumentSegment, ?> column, Object status) {
        return ChainWrappers.lambdaUpdateChain(documentSegmentService.getBaseMapper())
                .eq(DocumentSegment::getId, segmentId)
                .eq(DocumentSegment::getIndexVersion, expectedVersion)
                .set(column, status)
                .set(DocumentSegment::getFailReason, "")
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
                .set(KbDocument::getFailReason, "")
                .update();
    }

    /**
     * 失败落定到宿主行：任务失败时把宿主对应维度状态列置 FAIL 并记录带阶段前缀的原因
     * （vectorize: / graph:）。版本条件更新——版本已前进则本趟输出已作废，状态归新版本
     * 流程管，不落 FAIL 以免误报。段级宿主在 adi_document_segment，文档级在 adi_document。
     */
    private void markHostFailed(IndexTask task, String reason) {
        boolean graphical = DOC_INDEX_TYPE_GRAPHICAL.equals(task.getTaskType());
        String prefixed = StringUtils.abbreviate((graphical ? "graph: " : "vectorize: ") + reason, 500);
        if (TARGET_DOCUMENT.equals(task.getTargetType())) {
            KbDocument doc = kbDocumentMapper.getByUuid(task.getDocUuid());
            if (doc == null) {
                return;
            }
            ChainWrappers.lambdaUpdateChain(kbDocumentMapper)
                    .eq(KbDocument::getId, doc.getId())
                    .eq(KbDocument::getIndexVersion, task.getIndexVersion())
                    .set(graphical ? KbDocument::getGraphicalStatus : KbDocument::getEmbeddingStatus,
                            graphical ? GraphicalStatusEnum.FAIL : EmbeddingStatusEnum.FAIL)
                    .set(graphical ? KbDocument::getGraphicalStatusChangeTime : KbDocument::getEmbeddingStatusChangeTime,
                            java.time.LocalDateTime.now())
                    .set(KbDocument::getFailReason, prefixed)
                    .update();
            return;
        }
        DocumentSegment segment = documentSegmentService.lambdaQuery()
                .eq(DocumentSegment::getUuid, task.getSegmentUuid())
                .eq(DocumentSegment::getIsDeleted, false)
                .one();
        if (segment == null) {
            return;
        }
        ChainWrappers.lambdaUpdateChain(documentSegmentService.getBaseMapper())
                .eq(DocumentSegment::getId, segment.getId())
                .eq(DocumentSegment::getIndexVersion, task.getIndexVersion())
                .set(graphical ? DocumentSegment::getGraphicalStatus : DocumentSegment::getEmbeddingStatus,
                        graphical ? GraphicalStatusEnum.FAIL : EmbeddingStatusEnum.FAIL)
                .set(DocumentSegment::getFailReason, prefixed)
                .update();
    }

    private boolean versionAdvanced(Integer current, int snapshot) {
        return current != null && !Objects.equals(current, snapshot);
    }
}

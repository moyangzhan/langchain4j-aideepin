package com.moyz.adi.common.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.moyz.adi.common.entity.IndexTask;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface IndexTaskMapper extends BaseMapper<IndexTask> {

    /**
     * Merge-enqueue (conflict target with predicate, matching the partial unique index
     * uk_index_task_active, key includes index_version): same key same version pending merges
     * and refreshes the triggering user (debounces repeated same-version triggers); same key
     * same version failed revives in place as pending (manual retry); same key same version
     * running is untouched (that version is already executing); everything else inserts a new
     * row — a newer version is always a new key, a running row can never swallow it.
     * Older-version rows are cleaned up by {@link #supersede}.
     */
    void enqueue(@Param("task") IndexTask task);

    /**
     * Clear-and-stop-old when a newer version is enqueued (same key, unfinished rows with a
     * smaller version): pending is set to failed directly (no owning executor, safe; record the
     * cause); running only gets stop_flag (status untouched — the per-doc serial gate stays
     * closed until the executor aborts itself).
     * Races against claimOne are resolved atomically by the status guard.
     */
    int supersede(@Param("task") IndexTask task);

    /**
     * Atomically claim one pending task (runs inside a transaction):
     * at most one running per doc at any time — NOT EXISTS excludes docs that already have a
     * running row, pg_try_advisory_xact_lock closes the commit window of concurrently claiming
     * the same doc, FOR UPDATE SKIP LOCKED keeps multiple consumers from picking the same row.
     * The lock is released when the transaction ends. The claim writes start_time / heartbeat_time
     * as the baseline for the runtime heartbeat and resets stop_flag (revived/recovered rows
     * start fresh).
     */
    IndexTask claimOne();

    /**
     * Cancel every unfinished task of a document (document delete): pending fails in place,
     * running only gets stop_flag (executor aborts at its next checkpoint).
     */
    int cancelUnfinishedByDoc(@Param("docUuid") String docUuid, @Param("reason") String reason);

    /**
     * Finalize (done/failed). Guards: status='running' AND the claiming executor's epoch —
     * a row stale-reset and re-claimed under a new epoch must not be finalized by the previous
     * (zombie) executor. Returns 0 for exactly that case — the caller must not finalize the
     * host row either when its own task row was already taken over.
     */
    int finishOne(@Param("id") Long id, @Param("executorEpoch") String executorEpoch,
                  @Param("status") String status, @Param("failReason") String failReason);

    /**
     * Runtime heartbeat. Returns 0 = the row is no longer running under this executor's epoch
     * (recovered, force-failed or re-claimed); this executor is a zombie.
     */
    int heartbeat(@Param("id") Long id, @Param("executorEpoch") String executorEpoch);

    /**
     * Cooperative stop checkpoint: this row is still running under our epoch and carries the
     * stop flag set by a newer-version enqueue
     */
    boolean isStopFlagSet(@Param("id") Long id, @Param("executorEpoch") String executorEpoch);

    boolean hasRunningByDoc(@Param("docUuid") String docUuid);

    /**
     * Whether the doc has an unfinished (pending or running) task. Retry enqueues without
     * touching the doc status, so during the queue wait the doc still reads FAIL; the frontend
     * uses this to tell "queued/executing" apart from "finally failed".
     */
    boolean hasUnfinishedByDoc(@Param("docUuid") String docUuid);

    /**
     * Whether the doc has an unfinished DOCUMENT-level task (pending or running). Concurrent
     * segment edits are rejected while true: the doc task's snapshot would supersede their
     * output. Segment-level tasks are versioned per segment and excluded.
     */
    boolean hasUnfinishedDocTaskByDoc(@Param("docUuid") String docUuid);

    /**
     * Latest failed document-level task per task type (embedding/graphical). The doc row's
     * fail_reason keeps only the most recent failure and cannot express both dimensions
     * failing; the detail page's failure list reads per-dimension reasons from here.
     */
    List<IndexTask> listLatestFailedByDoc(@Param("docUuid") String docUuid);

    /**
     * Crash recovery: running rows whose heartbeat timed out are reset to pending for rerun
     */
    int resetStaleRunning(@Param("minutes") int minutes);

    /**
     * Hung circuit breaker: tasks whose start_time exceeds the max runtime while still
     * heartbeating are force-failed. Returns the broken rows so the caller can finalize
     * their host status columns in the same sweep (UPDATE..RETURNING via select tag).
     */
    List<IndexTask> failOverdue(@Param("minutes") int minutes);
}

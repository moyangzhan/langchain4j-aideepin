package com.moyz.adi.common.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.moyz.adi.common.entity.IndexTask;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

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
     * Finalize (done/failed). The status='running' guard: a row already stale-reset or
     * force-failed is no longer running; a late finish from a zombie executor must not
     * overwrite its status.
     */
    void finishOne(@Param("id") Long id, @Param("status") String status, @Param("failReason") String failReason);

    /**
     * Runtime heartbeat. Returns 0 = the row is no longer running (recovered or force-failed);
     * this executor is a zombie
     */
    int heartbeat(@Param("id") Long id);

    /**
     * Cooperative stop checkpoint: this row is still running and carries the stop flag set by
     * a newer-version enqueue
     */
    boolean isStopFlagSet(@Param("id") Long id);

    boolean hasRunningByDoc(@Param("docUuid") String docUuid);

    /**
     * Crash recovery: running rows whose heartbeat timed out are reset to pending for rerun
     */
    int resetStaleRunning(@Param("minutes") int minutes);

    /**
     * Hung circuit breaker: tasks whose start_time exceeds the max runtime while still
     * heartbeating are force-failed
     */
    int failOverdue(@Param("minutes") int minutes);
}

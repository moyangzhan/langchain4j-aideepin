package com.moyz.adi.common.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.moyz.adi.common.entity.IndexTask;
import org.apache.ibatis.annotations.Param;

public interface IndexTaskMapper extends BaseMapper<IndexTask> {

    /**
     * 合并入队：同键 pending 任务只更新版本与触发者（去抖）；撞上 running 不动
     * （由执行结束的版本比对+重入队兜底）
     */
    void enqueue(@Param("task") IndexTask task);

    /**
     * 原子领取一条 pending 任务（事务内执行）：
     * 同 doc 任一时刻只允许一个 running——NOT EXISTS 排除已有 running 的文档，
     * pg_try_advisory_xact_lock 堵住并发领取同 doc 的提交窗口，FOR UPDATE SKIP LOCKED
     * 防多消费者选中同一行。锁随事务结束释放。
     */
    IndexTask claimOne();

    void finishOne(@Param("id") Long id, @Param("status") String status, @Param("failReason") String failReason);

    boolean hasRunningByDoc(@Param("docUuid") String docUuid);

    int resetStaleRunning(@Param("minutes") int minutes);
}

package com.moyz.adi.common.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.moyz.adi.common.entity.IndexTask;
import org.apache.ibatis.annotations.Param;

public interface IndexTaskMapper extends BaseMapper<IndexTask> {

    /**
     * 合并入队（冲突目标带谓词，匹配部分唯一索引 uk_index_task_active，键含 index_version）：
     * 同键同版本 pending 合并刷新触发者（同版本重复触发去抖）；同键同版本 failed 原地复活
     * 为 pending（手动重试）；同键同版本 running 不动（该版本已在执行中）；其余情况插入新行
     * ——新版本必然是新键，running 行永远吞不掉它。旧版本行由 {@link #supersede} 清理。
     */
    void enqueue(@Param("task") IndexTask task);

    /**
     * 新版本入队时的清队与停旧（同键、版本更小的未完成行）：
     * pending 直接置 failed（无归属执行器，安全，记录死因）；running 仅打 stop_flag
     * （不动 status——同 doc 串行闸门必须等执行器自行中止后才打开）。
     * 与 claimOne 的竞态由 status 守卫原子化解。
     */
    int supersede(@Param("task") IndexTask task);

    /**
     * 原子领取一条 pending 任务（事务内执行）：
     * 同 doc 任一时刻只允许一个 running——NOT EXISTS 排除已有 running 的文档，
     * pg_try_advisory_xact_lock 堵住并发领取同 doc 的提交窗口，FOR UPDATE SKIP LOCKED
     * 防多消费者选中同一行。锁随事务结束释放。领取时写入 start_time / heartbeat_time
     * 作为执行期心跳的基准，并重置 stop_flag（复活/回收的行重新开始）。
     */
    IndexTask claimOne();

    /**
     * 终态落定（done/failed）。status='running' 守卫：被 stale 重置或超时强杀的行
     * 已不在 running，僵尸执行器迟到的 finish 不得覆盖其状态。
     */
    void finishOne(@Param("id") Long id, @Param("status") String status, @Param("failReason") String failReason);

    /**
     * 执行期心跳。返回 0 = 行已不处于 running（被回收/强杀），当前执行器已是僵尸
     */
    int heartbeat(@Param("id") Long id);

    /**
     * 协作式停止检查点：本行仍处 running 且被 newer-version enqueue 打了停止标志
     */
    boolean isStopFlagSet(@Param("id") Long id);

    boolean hasRunningByDoc(@Param("docUuid") String docUuid);

    /**
     * 进程崩溃回收：心跳超时的 running 行重置为 pending 等待重跑
     */
    int resetStaleRunning(@Param("minutes") int minutes);

    /**
     * 挂起熔断：start_time 超过最大执行时长且心跳仍存活的任务强制 failed
     */
    int failOverdue(@Param("minutes") int minutes);
}

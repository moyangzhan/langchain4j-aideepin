package com.moyz.adi.common.base;

import com.moyz.adi.common.annotation.DistributeLock;
import com.moyz.adi.common.cosntant.RedisKeyConstant;
import com.moyz.adi.common.service.KnowledgeBaseService;
import com.moyz.adi.common.service.ModelHealthService;
import com.moyz.adi.common.service.SysConfigService;
import com.moyz.adi.common.service.WorkflowRuntimeService;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public class Jobs {

    @Resource
    private KnowledgeBaseService knowledgeBaseService;

    @Resource
    private SysConfigService sysConfigService;

    @Resource
    private ModelHealthService modelHealthService;

    @Resource
    private WorkflowRuntimeService workflowRuntimeService;

    /**
     * WAITING_INPUT 回收阈值（分钟）：远大于人机交互的自然反馈间隔，且远大于实例重启窗口。
     * Expiry threshold for runtimes stuck waiting for user input, in minutes.
     */
    private static final int WAITING_INPUT_TIMEOUT_MINUTES = 30;

    /**
     * 多实例下 @Scheduled 每实例各触发一次——分布式锁仅保证同一时刻互斥（错峰触发的各实例仍会各执行一次，任务需幂等）。
     * <p>
     * Under multi-instance deployment every instance fires @Scheduled; the lock guarantees mutual exclusion at any instant only —
     * instances firing at staggered times still each run the job once per round, so tasks must be idempotent.
     * </p>
     */
    @DistributeLock(redisKey = RedisKeyConstant.LOCK_JOBS_HEALTH_CHECK, expireInSeconds = 600, continueIfAcquireFail = false)
    @Scheduled(initialDelay = 180_000, fixedDelay = 10 * 60_000)
    public void scheduledHealthCheck() {
        log.info("Scheduled health check starting");
        modelHealthService.checkAll();
    }

    /**
     * 定时刷新本实例的配置缓存。
     * Refresh this instance's LocalCache periodically.
     */
    @Scheduled(initialDelay = 10 * 60 * 1000, fixedDelay = 10 * 60_000)
    public void reloadConfig() {
        sysConfigService.loadAndCache();
    }

    @DistributeLock(redisKey = RedisKeyConstant.LOCK_JOBS_KB_STATISTIC, expireInSeconds = 300, continueIfAcquireFail = false)
    @Scheduled(fixedDelay = 60_000)
    public void run() {
        knowledgeBaseService.updateStatistic();
    }

    /**
     * 超时回收 WAITING_INPUT 的运行：置失败并清空中断快照。被回收的运行不可再续跑。
     * Expire runtimes stuck in WAITING_INPUT past the threshold: fail them and clear the checkpoint blob; an expired run is not resumable.
     */
    @DistributeLock(redisKey = RedisKeyConstant.LOCK_JOBS_WF_EXPIRE, expireInSeconds = 120, continueIfAcquireFail = false)
    @Scheduled(fixedDelay = 60_000)
    public void expireWaitingInput() {
        int expired = workflowRuntimeService.expireWaitingInput(WAITING_INPUT_TIMEOUT_MINUTES);
        if (expired > 0) {
            log.warn("Expired {} waiting-input workflow runtime(s) older than {} minutes", expired, WAITING_INPUT_TIMEOUT_MINUTES);
        }
    }
}

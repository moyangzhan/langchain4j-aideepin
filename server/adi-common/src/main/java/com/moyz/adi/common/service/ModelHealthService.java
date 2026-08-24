package com.moyz.adi.common.service;

import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.entity.ModelPlatform;
import com.moyz.adi.common.enums.ModelHealthStatus;
import com.moyz.adi.common.helper.LLMContext;
import com.moyz.adi.common.languagemodel.AbstractLLMService;
import dev.langchain4j.exception.AuthenticationException;
import dev.langchain4j.exception.ModelNotFoundException;
import org.apache.commons.lang3.StringUtils;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import jakarta.annotation.Resource;
import org.springframework.core.task.AsyncTaskExecutor;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

/**
 * 模型健康监测服务
 * <p>
 * 探测已启用模型的真实可用性。网络不通、欠费、API Key 失效等导致的不可用
 * 不会被静态标志 {@code AiModel.isEnable} 反映，本服务通过实际调用探测，
 * 连续失败 3 次后标记为 UNHEALTHY，对用户端不可见；认证/权限类错误（Key 失效、
 * 模型在提供方侧被停用）重试无意义，直接标记 UNHEALTHY。
 * </p>
 * <p>
 * Model health monitoring service — probes enabled models for real availability.
 * Models unavailable due to network issues, quota exhaustion, or API key failures
 * are marked UNHEALTHY after 3 consecutive failed probes and hidden from users;
 * auth-level errors (invalid key, model disabled at the provider) are marked
 * UNHEALTHY immediately.
 * </p>
 */
@Slf4j
@Service
public class ModelHealthService {

    private static final int FAILURE_THRESHOLD = 3;
    private static final int PROBE_TIMEOUT_SECONDS = 5;

    @Resource
    private AsyncTaskExecutor mainExecutor;

    /**
     * 模型健康缓存：key = modelName。
     * TTL 必须显著大于探测周期（10 分钟一轮）：连续失败计数与 UNHEALTHY 状态都依赖
     * 下一次探测能读到未过期的上次记录；TTL 仅作探测停摆时的兜底。
     * <p>
     * Model health cache: key = modelName. The TTL must be far larger than the probe
     * interval (10-minute rounds): both the consecutive-failure counter and the
     * UNHEALTHY state depend on the next probe reading a still-live previous entry;
     * the TTL only guards against the poller stalling.
     * </p>
     */
    private final Cache<String, HealthCheckResult> healthCache = CacheBuilder.newBuilder()
            .expireAfterWrite(1, TimeUnit.HOURS)
            .build();

    /**
     * 探测单个模型
     * <p>
     * Probe a single model. Success resets to HEALTHY, failure increments counter.
     * </p>
     *
     * @param service LLM service to probe
     */
    void check(AbstractLLMService service) {
        AiModel aiModel = service.getAiModel();
        String modelName = aiModel.getName();
        boolean success;
        String failReason = null;
        try {
            success = CompletableFuture.supplyAsync(service::performHealthCheck, mainExecutor)
                    .get(PROBE_TIMEOUT_SECONDS, TimeUnit.SECONDS);
        } catch (Exception e) {
            success = false;
            failReason = e.getCause() != null ? e.getCause().getMessage() : e.getMessage();
        }

        HealthCheckResult previous = healthCache.getIfPresent(modelName);
        int consecutiveFailures = 0;
        if (success) {
            // 任何一次成功 → 立即恢复 HEALTHY
            log.info("Health check passed for model:{}", modelName);
        } else {
            consecutiveFailures = (previous != null ? previous.getConsecutiveFailures() : 0) + 1;
            log.warn("Health check failed for model:{} ({} of {}), reason:{}",
                    modelName, consecutiveFailures, FAILURE_THRESHOLD, failReason);
        }
        ModelHealthStatus status;
        if (success) {
            status = ModelHealthStatus.HEALTHY;
        } else if (consecutiveFailures >= FAILURE_THRESHOLD) {
            status = ModelHealthStatus.UNHEALTHY;
        } else {
            status = previous != null ? previous.getStatus() : ModelHealthStatus.HEALTHY;
        }

        healthCache.put(modelName, HealthCheckResult.builder()
                .status(status)
                .consecutiveFailures(consecutiveFailures)
                .lastCheckTime(System.currentTimeMillis())
                .failReason(failReason)
                .build());
    }

    /**
     * 探测所有已启用且有 API Key 的模型
     * <p>
     * Probe all models that are enabled and have an API key configured.
     * </p>
     */
    public void checkAll() {
        List<AbstractLLMService> services = LLMContext.getAllServices();
        if (services.isEmpty()) {
            return;
        }
        log.info("Starting health check for {} models", services.size());
        for (AbstractLLMService service : services) {
            if (shouldProbe(service)) {
                check(service);
            }
        }
        log.info("Health check completed");
    }

    /**
     * 判断模型是否需要探测：已启用 且 API Key 不为空
     */
    private boolean shouldProbe(AbstractLLMService service) {
        AiModel aiModel = service.getAiModel();
        if (!Boolean.TRUE.equals(aiModel.getIsEnable())) {
            return false;
        }
        ModelPlatform platform = service.getPlatform();
        return platform != null && StringUtils.isNotBlank(platform.getApiKey());
    }

    /**
     * 查询模型健康状态
     * <p>
     * Get health status for a model. Enabled models with an API key default to
     * HEALTHY until proven otherwise by health checks.
     * </p>
     *
     * @param modelName 模型名称 / Model name
     * @return 健康状态 / Health status
     */
    public ModelHealthStatus getStatus(String modelName) {
        HealthCheckResult result = healthCache.getIfPresent(modelName);
        if (result != null) {
            return result.getStatus();
        }
        return defaultHealthy(modelName) ? ModelHealthStatus.HEALTHY : ModelHealthStatus.UNHEALTHY;
    }

    /**
     * 模型是否健康（供 LLMContext 调用）
     * <p>
     * Check if model is healthy (for LLMContext filtering).
     * Enabled models with API key default to healthy before first probe.
     * </p>
     *
     * @param modelName 模型名称 / Model name
     * @return true if healthy, false if confirmed unhealthy
     */
    public boolean isHealthy(String modelName) {
        return getStatus(modelName) != ModelHealthStatus.UNHEALTHY;
    }

    /**
     * Record a failed invocation for a model, triggered when a user's real
     * LLM call fails (network error, API error, etc.).
     * <p>
     * After {@link #FAILURE_THRESHOLD} consecutive failures without a
     * successful probe, the model is marked UNHEALTHY.
     * </p>
     *
     * @param modelName  Model name
     * @param failReason Failure reason (exception message)
     */
    public void recordFailure(String modelName, String failReason) {
        HealthCheckResult previous = healthCache.getIfPresent(modelName);
        int consecutiveFailures = (previous != null ? previous.getConsecutiveFailures() : 0) + 1;
        ModelHealthStatus status;
        if (consecutiveFailures >= FAILURE_THRESHOLD) {
            status = ModelHealthStatus.UNHEALTHY;
            log.warn("Model {} marked UNHEALTHY after {} consecutive failures, reason:{}",
                    modelName, consecutiveFailures, failReason);
        } else {
            status = previous != null ? previous.getStatus() : ModelHealthStatus.HEALTHY;
            log.info("Model {} invocation failed ({} of {}), reason:{}",
                    modelName, consecutiveFailures, FAILURE_THRESHOLD, failReason);
        }
        healthCache.put(modelName, HealthCheckResult.builder()
                .status(status)
                .consecutiveFailures(consecutiveFailures)
                .lastCheckTime(System.currentTimeMillis())
                .failReason(failReason)
                .build());
    }

    /**
     * 记录一次真实调用失败（传入异常以分类）：认证/权限类错误（Key 失效、模型在提供方
     * 侧被停用）重试必然失败，直接标记 UNHEALTHY，不进连续失败计数；其余异常走
     * {@link #recordFailure(String, String)} 的计数逻辑。
     * <p>
     * Record a failed invocation with the exception for classification: auth-level
     * errors (invalid key, model disabled at the provider) can never succeed on retry
     * and mark UNHEALTHY immediately, bypassing the consecutive-failure counter;
     * other errors go through {@link #recordFailure(String, String)}.
     * </p>
     *
     * @param modelName 模型名称 / Model name
     * @param error     调用抛出的异常 / Exception thrown by the invocation
     */
    public void recordFailure(String modelName, Throwable error) {
        String reason = error.getCause() != null ? error.getCause().getMessage() : error.getMessage();
        if (isPermanentModelError(error)) {
            healthCache.put(modelName, HealthCheckResult.builder()
                    .status(ModelHealthStatus.UNHEALTHY)
                    .consecutiveFailures(FAILURE_THRESHOLD)
                    .lastCheckTime(System.currentTimeMillis())
                    .failReason(reason)
                    .build());
            log.warn("Model {} marked UNHEALTHY immediately, permanent auth error:{}", modelName, reason);
            return;
        }
        recordFailure(modelName, reason);
    }

    /**
     * 沿因果链判断是否模型级永久错误（认证失败 / 模型在提供方侧不存在或被停用），
     * 带自引用环保护；请求级错误（如参数错误）不算，模型本身可能正常
     */
    private static boolean isPermanentModelError(Throwable error) {
        Throwable current = error;
        while (current != null) {
            if (current instanceof AuthenticationException || current instanceof ModelNotFoundException) {
                return true;
            }
            Throwable cause = current.getCause();
            if (cause == current) {
                break;
            }
            current = cause;
        }
        return false;
    }

    /**
     * 未探测过的模型是否应默认为 HEALTHY
     * 条件：已启用 且 platform apiKey 不为空
     */
    private boolean defaultHealthy(String modelName) {
        for (AbstractLLMService service : LLMContext.getAllServices()) {
            if (service.getAiModel().getName().equalsIgnoreCase(modelName)) {
                return shouldProbe(service);
            }
        }
        return false;
    }

    /**
     * 获取所有模型状态（供管理端 API）
     * <p>
     * Get all model health statuses (for admin API).
     * </p>
     *
     * @return map of modelName → HealthCheckResult
     */
    public Map<String, HealthCheckResult> getAllStatuses() {
        Map<String, HealthCheckResult> result = new HashMap<>();
        for (AbstractLLMService service : LLMContext.getAllServices()) {
            String modelName = service.getAiModel().getName();
            HealthCheckResult cached = healthCache.getIfPresent(modelName);
            if (cached != null) {
                result.put(modelName, cached);
            } else {
                boolean healthy = shouldProbe(service);
                result.put(modelName, HealthCheckResult.builder()
                        .status(healthy ? ModelHealthStatus.HEALTHY : ModelHealthStatus.UNHEALTHY)
                        .consecutiveFailures(0)
                        .lastCheckTime(0)
                        .build());
            }
        }
        return result;
    }

    // ==================== 内部类 ====================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class HealthCheckResult {
        /**
         * 健康状态 / Health status
         */
        private ModelHealthStatus status;
        /**
         * 连续失败次数 / Consecutive failure count
         */
        private int consecutiveFailures;
        /**
         * 上次探测时间（毫秒时间戳）/ Last check time (ms timestamp)
         */
        private long lastCheckTime;
        /**
         * 失败原因 / Failure reason
         */
        private String failReason;
    }
}

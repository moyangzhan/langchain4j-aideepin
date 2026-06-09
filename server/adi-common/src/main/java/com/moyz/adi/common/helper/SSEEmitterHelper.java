package com.moyz.adi.common.helper;

import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.cosntant.RedisKeyConstant;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.interfaces.TriConsumer;
import com.moyz.adi.common.languagemodel.data.LLMResponseContent;
import com.moyz.adi.common.util.*;
import com.moyz.adi.common.vo.*;
import dev.langchain4j.model.chat.response.ChatResponse;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.io.IOException;
import java.util.Map;
import java.text.MessageFormat;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

/**
 * SSE 事件发送辅助类
 * <p>
 * 通过 {@link SseEmitterRegistry} 管理 uuid → SseEmitter 映射，
 * 所有方法以 SSE 请求标识(uuid)作为参数，替代直接传递 SseEmitter。
 * </p>
 * <p>
 * SSE event helper — uses {@link SseEmitterRegistry} for uuid → SseEmitter mapping.
 * All methods accept SSE request identifier (uuid) instead of SseEmitter directly.
 * </p>
 */
@Slf4j
@Service
public class SSEEmitterHelper {

    /**
     * Web 用户最大并发 SSE 数 / Max concurrent SSE for web users
     */
    private static final int MAX_WEB_CONCURRENT = 1;
    /**
     * API 用户最大并发 SSE 数 / Max concurrent SSE for API users
     */
    private static final int MAX_API_CONCURRENT = 5;

    @Resource
    private StringRedisTemplate stringRedisTemplate;

    @Resource
    private RateLimitHelper rateLimitHelper;

    @Resource
    private SseEmitterRegistry registry;

    /**
     * 已完成的 SseEmitter 缓存，防止重复完成 / Cache for completed SseEmitters to prevent double-completion
     */
    private static final Cache<SseEmitter, Boolean> COMPLETED_SSE = CacheBuilder.newBuilder().expireAfterWrite(10, TimeUnit.MINUTES).build();

    // ==================== 请求入口（check + start） ====================

    /**
     * 检查请求是否允许（限流 + 并发数检查），不允许则直接发送错误并完成 SSE
     * <p>
     * Check if request is allowed (rate limit + concurrency check).
     * If not allowed, sends error and completes the SSE.
     * </p>
     *
     * @param user       用户 / User
     * @param sseUuid    SSE 请求标识 / SSE request identifier
     * @param sseEmitter SseEmitter 实例 / SseEmitter instance
     * @return true=允许 / allowed, false=已拒绝 / rejected
     */
    public boolean checkOrComplete(User user, String sseUuid, SseEmitter sseEmitter) {
        //Check: rate limit
        String requestTimesKey = MessageFormat.format(RedisKeyConstant.USER_REQUEST_TEXT_TIMES, user.getId());
        if (!rateLimitHelper.checkRequestTimes(requestTimesKey, LocalCache.TEXT_RATE_LIMIT_CONFIG)) {
            doSendErrorAndComplete(user.getId(), sseUuid, sseEmitter, SpringUtil.getMessage(ErrorEnum.A_REQUEST_TOO_MUCH.getInfo()));
            return false;
        }

        //Check: concurrent SSE count
        String activeKey = MessageFormat.format(RedisKeyConstant.USER_ACTIVE_SSE_COUNT, user.getId());
        stringRedisTemplate.opsForSet().add(activeKey, sseUuid);
        //每次添加都刷新 TTL = 6min（略大于 SseEmitter 5min timeout），防止 crash 导致集合永不清理
        stringRedisTemplate.expire(activeKey, 6, TimeUnit.MINUTES);
        Long size = stringRedisTemplate.opsForSet().size(activeKey);
        boolean isApi = ThreadContext.isExtApiRequest();
        int maxConcurrent = isApi ? MAX_API_CONCURRENT : MAX_WEB_CONCURRENT;
        if (size != null && size > maxConcurrent) {
            stringRedisTemplate.opsForSet().remove(activeKey, sseUuid);
            doSendErrorAndComplete(user.getId(), sseUuid, sseEmitter, SpringUtil.getMessage("SSE_RESPONDING"));
            return false;
        }
        return true;
    }

    /**
     * 启动 SSE 流，发送 START 事件（从注册中心查找 emitter）
     * <p>
     * Start SSE stream, send START event (look up emitter from registry).
     * </p>
     */
    public void startSse(User user, String sseUuid) {
        startSse(user, sseUuid, registry.get(sseUuid), null);
    }

    /**
     * 启动 SSE 流，发送 START 事件（从注册中心查找 emitter）
     * <p>
     * Start SSE stream, send START event (look up emitter from registry).
     * </p>
     */
    public void startSse(User user, String sseUuid, String data) {
        startSse(user, sseUuid, registry.get(sseUuid), data);
    }

    public void startSse(User user, String sseUuid, SseEmitter sseEmitter, String data) {
        registry.register(sseUuid, sseEmitter);

        String requestTimesKey = MessageFormat.format(RedisKeyConstant.USER_REQUEST_TEXT_TIMES, user.getId());
        rateLimitHelper.increaseRequestTimes(requestTimesKey, LocalCache.TEXT_RATE_LIMIT_CONFIG);
        try {
            SseEmitter.SseEventBuilder builder = SseEmitter.event().name(AdiConstant.SSEEventName.START);
            if (StringUtils.isNotBlank(data)) {
                builder.data(data);
            }
            sseEmitter.send(builder);
        } catch (IOException e) {
            log.error("startSse error", e);
            sseEmitter.completeWithError(e);
            COMPLETED_SSE.put(sseEmitter, Boolean.TRUE);
            registry.unregister(sseUuid);
            decActiveSseCount(user.getId(), sseUuid);
        }
    }

    // ==================== 流式调用入口 ====================

    /**
     * event_stream 请求，完成后关闭 sse 并执行回调
     * <p>
     * event_stream request: close SSE and execute callback after completion.
     * </p>
     *
     * @param sseAskParams     请求参数（必须包含 sseUuid）/ Request parameters (must include sseUuid)
     * @param completeCallback 请求结束后的回调 / Callback after request completion
     */
    public void call(SseAskParams sseAskParams, TriConsumer<LLMResponseContent, PromptMeta, AnswerMeta> completeCallback) {
        String sseUuid = sseAskParams.getSseUuid();
        registerEventStreamListener(sseAskParams);
        LLMContext.getServiceOrDefault(sseAskParams.getModelPlatform(), sseAskParams.getModelName()).streamingChat(sseAskParams, (response, promptMeta, answerMeta) -> {
            try {
                completeCallback.accept(response, promptMeta, answerMeta);
            } catch (Exception e) {
                log.error("commonProcess error", e);
                errorAndShutdown(e, sseUuid);
            } finally {
                SseEmitter sseEmitter = registry.get(sseUuid);
                if (sseEmitter != null) {
                    COMPLETED_SSE.put(sseEmitter, Boolean.TRUE);
                }
                registry.unregister(sseUuid);
                decActiveSseCount(sseAskParams.getUser().getId(), sseUuid);
            }
        });
    }

    /**
     * 注册 event stream 的生命周期事件（onCompletion/onTimeout/onError）
     * <p>
     * Register lifecycle events for the event stream.
     * </p>
     *
     * @param sseAskParams 参数（必须包含 sseUuid）/ Parameters (must include sseUuid)
     */
    public void registerEventStreamListener(SseAskParams sseAskParams) {
        User user = sseAskParams.getUser();
        String sseUuid = sseAskParams.getSseUuid();
        SseEmitter sseEmitter = registry.get(sseUuid);
        if (sseEmitter == null) {
            log.error("registerEventStreamListener: SseEmitter not found for sseUuid:{}", sseUuid);
            return;
        }
        sseEmitter.onCompletion(() -> {
            log.info("response complete,uid:{}", user.getId());
            cleanupEmitter(sseUuid, sseEmitter, user.getId());
        });
        sseEmitter.onTimeout(() -> {
            log.warn("sseEmitter timeout,uid:{},on timeout:{}", user.getId(), sseEmitter.getTimeout());
            cleanupEmitter(sseUuid, sseEmitter, user.getId());
        });
        sseEmitter.onError(
                throwable -> {
                    try {
                        log.error("sseEmitter error,uid:{},on error", user.getId(), throwable);
                        sseEmitter.send(SseEmitter.event().name(AdiConstant.SSEEventName.ERROR).data(throwable.getMessage()));
                    } catch (IOException e) {
                        log.error("error", e);
                    } finally {
                        cleanupEmitter(sseUuid, sseEmitter, user.getId());
                    }
                }
        );
    }

    // ==================== 实例方法（接受 uuid，内部查找 emitter） ====================

    public void sendComplete(long userId, String sseUuid, String msg) {
        SseEmitter sseEmitter = registry.get(sseUuid);
        if (sseEmitter == null || Boolean.TRUE.equals(COMPLETED_SSE.getIfPresent(sseEmitter))) {
            log.warn("sseEmitter already completed or not found,userId:{}", userId);
            decActiveSseCount(userId, sseUuid);
            return;
        }
        try {
            sseEmitter.send(SseEmitter.event().name(AdiConstant.SSEEventName.DONE).data(msg));
        } catch (IOException e) {
            throw new RuntimeException(e);
        } finally {
            cleanupEmitter(sseUuid, sseEmitter, userId);
        }
    }

    public void sendComplete(long userId, String sseUuid, PromptMeta questionMeta, AnswerMeta answerMeta, AudioInfo audioInfo) {
        ChatMeta chatMeta = new ChatMeta(questionMeta, answerMeta, audioInfo);
        String meta = JsonUtil.toJson(chatMeta).replace("\r\n", "");
        this.sendComplete(userId, sseUuid, " " + AdiConstant.SSEEventName.META + meta);
    }

    /**
     * 关闭 sse
     * <p>
     * Close SSE.
     * </p>
     *
     * @param userId  用户id / User ID
     * @param sseUuid SSE 请求标识 / SSE request identifier
     */
    public void sendComplete(long userId, String sseUuid) {
        SseEmitter sseEmitter = registry.get(sseUuid);
        if (sseEmitter == null || Boolean.TRUE.equals(COMPLETED_SSE.getIfPresent(sseEmitter))) {
            log.warn("sseEmitter already completed or not found,userId:{}", userId);
            decActiveSseCount(userId, sseUuid);
            return;
        }
        try {
            sseEmitter.send(SseEmitter.event().name(AdiConstant.SSEEventName.DONE));
            sseEmitter.complete();
        } catch (Exception e) {
            log.warn("sendComplete error", e);
        } finally {
            cleanupEmitter(sseUuid, sseEmitter, userId);
        }
    }

    public void sendStartAndComplete(long userId, String sseUuid, String msg) {
        SseEmitter sseEmitter = registry.get(sseUuid);
        if (sseEmitter == null) {
            log.warn("sendStartAndComplete: SseEmitter not found for sseUuid:{}", sseUuid);
            decActiveSseCount(userId, sseUuid);
            return;
        }
        try {
            sseEmitter.send(SseEmitter.event().name(AdiConstant.SSEEventName.START));
            sseEmitter.send(SseEmitter.event().name(AdiConstant.SSEEventName.DONE).data(msg));
        } catch (IOException e) {
            throw new RuntimeException(e);
        } finally {
            cleanupEmitter(sseUuid, sseEmitter, userId);
        }
    }

    public void sendErrorAndComplete(long userId, String sseUuid, String errorMsg) {
        SseEmitter sseEmitter = registry.get(sseUuid);
        doSendErrorAndComplete(userId, sseUuid, sseEmitter, errorMsg);
    }

    // ==================== 静态方法（接受 uuid，通过 SpringUtil 获取 registry） ====================

    public static void parseAndSendPartialMsg(String uuid, String content) {
        parseAndSendPartialMsg(uuid, "", content);
    }

    public static void sendAudio(String uuid, Object content) {
        SseEmitter sseEmitter = getEmitter(uuid);
        if (sseEmitter == null) return;
        try {
            sseEmitter.send(SseEmitter.event().name(AdiConstant.SSEEventName.AUDIO).data(content));
        } catch (IOException e) {
            log.error("stream onNext error", e);
            throw new RuntimeException(e);
        }
    }

    public static void sendThinking(String uuid, String content) {
        SseEmitter sseEmitter = getEmitter(uuid);
        if (sseEmitter == null) return;
        try {
            sseEmitter.send(SseEmitter.event().name(AdiConstant.SSEEventName.THINKING).data(content));
        } catch (IOException e) {
            log.error("stream onNext error", e);
            throw new RuntimeException(e);
        }
    }

    public static void sendToolCall(String uuid, String toolName, long durationMs, boolean success) {
        SseEmitter sseEmitter = getEmitter(uuid);
        if (sseEmitter == null) {
            return;
        }
        if (Boolean.TRUE.equals(COMPLETED_SSE.getIfPresent(sseEmitter))) {
            log.warn("sseEmitter already completed, skip sendToolCall");
            return;
        }
        try {
            String safeName = toolName != null ? toolName : "unknown";
            String data = JsonUtil.toJson(Map.of("toolName", safeName, "durationMs", durationMs, "success", success));
            sseEmitter.send(SseEmitter.event().name(AdiConstant.SSEEventName.TOOL_CALL).data(data));
        } catch (Exception e) {
            log.error("sendToolCall error", e);
            COMPLETED_SSE.put(sseEmitter, Boolean.TRUE);
        }
    }

    public static void parseAndSendPartialMsg(String uuid, String name, String content) {
        SseEmitter sseEmitter = getEmitter(uuid);
        if (sseEmitter == null) return;
        if (Boolean.TRUE.equals(COMPLETED_SSE.getIfPresent(sseEmitter))) {
            log.warn("sseEmitter already completed,name:{}", name);
            return;
        }
        String[] lines = content.split("[\\r\\n]", -1);
        if (lines.length > 1) {
            sendPartial(uuid, name, " " + lines[0]);
            for (int i = 1; i < lines.length; i++) {
                sendPartial(uuid, name, "-_wrap_-");
                sendPartial(uuid, name, " " + lines[i]);
            }
        } else {
            sendPartial(uuid, name, " " + content);
        }
    }

    public static void sendPartial(String uuid, String name, String msg) {
        SseEmitter sseEmitter = getEmitter(uuid);
        if (sseEmitter == null) return;
        if (Boolean.TRUE.equals(COMPLETED_SSE.getIfPresent(sseEmitter))) {
            log.warn("sseEmitter already completed,name:{}", name);
            return;
        }
        try {
            if (StringUtils.isNotBlank(name)) {
                sseEmitter.send(SseEmitter.event().name(name).data(msg));
            } else {
                sseEmitter.send(msg);
            }
        } catch (IOException ioException) {
            log.error("stream onNext error", ioException);
            COMPLETED_SSE.put(sseEmitter, Boolean.TRUE);
            throw new RuntimeException(ioException);
        }
    }

    public static void errorAndShutdown(Throwable error, String uuid) {
        SseEmitter sseEmitter = getEmitter(uuid);
        if (sseEmitter == null) return;
        if (Boolean.TRUE.equals(COMPLETED_SSE.getIfPresent(sseEmitter))) {
            log.warn("sseEmitter already completed,ignore error:{}", error.getMessage());
            return;
        }
        log.error("stream error", error);
        try {
            sseEmitter.send(SseEmitter.event().name(AdiConstant.SSEEventName.ERROR).data(error.getMessage()));
        } catch (IOException e) {
            log.error("sse error", e);
        } finally {
            COMPLETED_SSE.put(sseEmitter, Boolean.TRUE);
            sseEmitter.complete();
            //自包含清理兜底：如果 onCompletion 回调未注册（registerEventStreamListener 未调用前出错），
            //这里主动执行 unregister + decActiveSseCount，确保 Redis 并发计数不泄漏
            SseEmitterRegistry reg = SpringUtil.getBean(SseEmitterRegistry.class);
            reg.unregister(uuid);
            //decActiveSseCount 由 onCompletion 回调处理（registerEventStreamListener 在所有调用方中都先于 errorAndShutdown）
            //6min TTL 作为最终兜底
        }
    }

    // ==================== 不涉及 SseEmitter 的工具方法 ====================

    /**
     * 计算 llm 返回消费的 token
     * <p>
     * Calculate tokens consumed by LLM response.
     * </p>
     */
    public static Pair<PromptMeta, AnswerMeta> calculateToken(ChatResponse response, String uuid) {
        log.info("Response data completed:{}", response);
        int inputTokenCount = 0;
        int outputTokenCount = 0;
        if (response.metadata() != null && response.metadata().tokenUsage() != null) {
            Integer input = response.metadata().tokenUsage().inputTokenCount();
            Integer output = response.metadata().tokenUsage().outputTokenCount();
            inputTokenCount = input != null ? input : 0;
            outputTokenCount = output != null ? output : 0;
        }
        log.info("StreamingChatModel token cost,uuid:{},inputTokenCount:{},outputTokenCount:{}", uuid, inputTokenCount, outputTokenCount);
        LLMTokenUtil.cacheTokenUsage(SpringUtil.getBean(StringRedisTemplate.class), uuid, response.metadata().tokenUsage());

        PromptMeta questionMeta = new PromptMeta(inputTokenCount, uuid);
        AnswerMeta answerMeta = AnswerMeta.builder().tokens(outputTokenCount).uuid(UuidUtil.createShort()).build();
        return Pair.of(questionMeta, answerMeta);
    }

    public void deleteCache(String cache) {
        stringRedisTemplate.delete(cache);
    }

    // ==================== 内部方法 ====================

    /**
     * 通过 uuid 从注册中心获取 SseEmitter
     * <p>
     * Look up SseEmitter by uuid from the registry.
     * </p>
     */
    private static SseEmitter getEmitter(String uuid) {
        return SpringUtil.getBean(SseEmitterRegistry.class).get(uuid);
    }

    /**
     * 统一的清理方法：标记完成 + 注销 + 递减并发计数（幂等）
     * <p>
     * Unified cleanup: mark completed + unregister + decrement active count (idempotent).
     * COMPLETED_SSE guard prevents double-cleanup from call() finally vs onCompletion callback.
     * </p>
     */
    private void cleanupEmitter(String sseUuid, SseEmitter sseEmitter, long userId) {
        if (sseEmitter != null && Boolean.TRUE.equals(COMPLETED_SSE.getIfPresent(sseEmitter))) {
            //已清理过，跳过（call() finally 和 onCompletion 回调可能都触发）
            return;
        }
        if (sseEmitter != null) {
            COMPLETED_SSE.put(sseEmitter, Boolean.TRUE);
        }
        registry.unregister(sseUuid);
        decActiveSseCount(userId, sseUuid);
    }

    /**
     * 发送错误并完成（内部实现，允许 sseEmitter 为 null）
     * <p>
     * Send error and complete (internal impl, allows null sseEmitter).
     * </p>
     */
    private void doSendErrorAndComplete(long userId, String sseUuid, SseEmitter sseEmitter, String errorMsg) {
        if (sseEmitter != null && Boolean.TRUE.equals(COMPLETED_SSE.getIfPresent(sseEmitter))) {
            log.warn("sseEmitter already completed,ignore error:{}", errorMsg);
            decActiveSseCount(userId, sseUuid);
            return;
        }
        if (sseEmitter != null) {
            try {
                sseEmitter.send(SseEmitter.event().name(AdiConstant.SSEEventName.ERROR).data(Objects.toString(errorMsg, "")));
            } catch (IOException e) {
                log.warn("sendErrorAndComplete userId:{},errorMsg:{}", userId, errorMsg);
                throw new RuntimeException(e);
            } finally {
                cleanupEmitter(sseUuid, sseEmitter, userId);
            }
        } else {
            decActiveSseCount(userId, sseUuid);
        }
    }

    /**
     * 递减用户活跃 SSE 并发计数
     * <p>
     * Decrement user active SSE concurrency count (Redis SET SREM, idempotent).
     * </p>
     */
    private void decActiveSseCount(long userId, String sseUuid) {
        String activeKey = MessageFormat.format(RedisKeyConstant.USER_ACTIVE_SSE_COUNT, userId);
        stringRedisTemplate.opsForSet().remove(activeKey, sseUuid);
    }
}

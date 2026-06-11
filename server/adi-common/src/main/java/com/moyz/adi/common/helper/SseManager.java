package com.moyz.adi.common.helper;

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
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;

/**
 * SSE 管理器
 * <p>
 * 统一管理 SSE 请求的完整生命周期：注册、限流、事件发送、清理。
 * 内部通过 {@link SseEntry} 同时追踪 SseEmitter 实例和关联的 userId，
 * entry 从 map 中移除即代表已完成，天然防止重复完成。
 * </p>
 * <p>
 * SSE manager — unified lifecycle management for SSE connections: registration,
 * rate limiting, event dispatching, and cleanup. The internal {@link SseEntry}
 * tracks both the emitter and its associated userId; removing the entry from
 * the map signals completion, providing natural double-completion prevention.
 * </p>
 */
@Slf4j
@Service
public class SseManager {

    /**
     * 内部条目：emitter + userId 一体
     * <p>
     * Internal entry bundling an emitter with its owning userId.
     * </p>
     */
    private record SseEntry(SseEmitter emitter, long userId) {
    }

    private final ConcurrentHashMap<String, SseEntry> entries = new ConcurrentHashMap<>();

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

    // ==================== 注册/查找/注销 ====================

    /**
     * 注册 SseEmitter 并关联 userId
     * <p>
     * Register an emitter associated with a userId.
     * </p>
     *
     * @param uuid    SSE 请求标识 / SSE request identifier
     * @param emitter SseEmitter 实例 / SseEmitter instance
     * @param userId  用户 ID / User ID
     */
    public void register(String uuid, SseEmitter emitter, long userId) {
        entries.put(uuid, new SseEntry(emitter, userId));
    }

    /**
     * 注册 SseEmitter（无 userId，仅 blocking 模式兼容）
     * <p>
     * Register an emitter without userId (blocking mode compatibility).
     * </p>
     *
     * @param uuid    SSE 请求标识 / SSE request identifier
     * @param emitter SseEmitter 实例 / SseEmitter instance
     */
    public void register(String uuid, SseEmitter emitter) {
        entries.put(uuid, new SseEntry(emitter, -1));
    }

    /**
     * 获取 SseEmitter
     * <p>
     * Get the SseEmitter for the given uuid.
     * </p>
     *
     * @param uuid SSE 请求标识 / SSE request identifier
     * @return SseEmitter 实例，不存在则返回 null / SseEmitter or null
     */
    public SseEmitter get(String uuid) {
        SseEntry entry = entries.get(uuid);
        return entry != null ? entry.emitter() : null;
    }

    /**
     * 获取关联的 userId
     * <p>
     * Get the userId associated with the given uuid.
     * </p>
     *
     * @param uuid SSE 请求标识 / SSE request identifier
     * @return userId，不存在则返回 null / userId or null
     */
    public Long getUserId(String uuid) {
        SseEntry entry = entries.get(uuid);
        return entry != null ? entry.userId() : null;
    }

    /**
     * 注销 SseEmitter 并递减 Redis 并发计数（幂等）
     * <p>
     * Unregister the emitter and decrement the Redis concurrency counter (idempotent).
     * </p>
     *
     * @param uuid SSE 请求标识 / SSE request identifier
     */
    public void unregister(String uuid) {
        SseEntry removed = entries.remove(uuid);
        if (removed != null && removed.userId() > 0) {
            decActiveSseCount(removed.userId(), uuid);
        }
    }

    /**
     * 判断是否已完成（entry 不存在 = 已完成）
     * <p>
     * Check if the emitter has been completed (entry absent = completed).
     * </p>
     */
    public boolean isCompleted(String uuid) {
        return !entries.containsKey(uuid);
    }

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
     * 启动 SSE 流，发送 START 事件（从管理器查找 emitter）
     * <p>
     * Start SSE stream, send START event (look up emitter from manager).
     * </p>
     */
    public void startSse(User user, String sseUuid) {
        startSse(user, sseUuid, get(sseUuid), null);
    }

    /**
     * 启动 SSE 流，发送 START 事件（从管理器查找 emitter）
     * <p>
     * Start SSE stream, send START event (look up emitter from manager).
     * </p>
     */
    public void startSse(User user, String sseUuid, String data) {
        startSse(user, sseUuid, get(sseUuid), data);
    }

    public void startSse(User user, String sseUuid, SseEmitter sseEmitter, String data) {
        register(sseUuid, sseEmitter, user.getId());

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
            unregister(sseUuid);
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
                unregister(sseUuid);
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
        SseEmitter sseEmitter = get(sseUuid);
        if (sseEmitter == null) {
            log.error("registerEventStreamListener: SseEmitter not found for sseUuid:{}", sseUuid);
            return;
        }
        sseEmitter.onCompletion(() -> {
            log.info("response complete,uid:{}", user.getId());
            unregister(sseUuid);
        });
        sseEmitter.onTimeout(() -> {
            log.warn("sseEmitter timeout,uid:{},on timeout:{}", user.getId(), sseEmitter.getTimeout());
            unregister(sseUuid);
        });
        sseEmitter.onError(
                throwable -> {
                    try {
                        log.error("sseEmitter error,uid:{},on error", user.getId(), throwable);
                        sseEmitter.send(SseEmitter.event().name(AdiConstant.SSEEventName.ERROR).data(throwable.getMessage()));
                    } catch (IOException e) {
                        log.error("error", e);
                    } finally {
                        unregister(sseUuid);
                    }
                }
        );
    }

    // ==================== 实例方法（接受 uuid，内部查找 emitter） ====================

    public void sendComplete(long userId, String sseUuid, String msg) {
        SseEntry entry = entries.get(sseUuid);
        if (entry == null) {
            log.warn("sseEmitter already completed or not found,userId:{}", userId);
            return;
        }
        try {
            entry.emitter().send(SseEmitter.event().name(AdiConstant.SSEEventName.DONE).data(msg));
            entry.emitter().complete();
        } catch (Exception e) {
            log.warn("sendComplete error,userId:{}", userId, e);
        } finally {
            unregister(sseUuid);
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
        SseEntry entry = entries.get(sseUuid);
        if (entry == null) {
            log.warn("sseEmitter already completed or not found,userId:{}", userId);
            return;
        }
        try {
            entry.emitter().send(SseEmitter.event().name(AdiConstant.SSEEventName.DONE));
            entry.emitter().complete();
        } catch (Exception e) {
            log.warn("sendComplete error", e);
        } finally {
            unregister(sseUuid);
        }
    }

    public void sendStartAndComplete(long userId, String sseUuid, String msg) {
        SseEntry entry = entries.get(sseUuid);
        if (entry == null) {
            log.warn("sendStartAndComplete: SseEmitter not found for sseUuid:{}", sseUuid);
            return;
        }
        try {
            entry.emitter().send(SseEmitter.event().name(AdiConstant.SSEEventName.START));
            entry.emitter().send(SseEmitter.event().name(AdiConstant.SSEEventName.DONE).data(msg));
            entry.emitter().complete();
        } catch (Exception e) {
            log.warn("sendStartAndComplete error,userId:{}", userId, e);
        } finally {
            unregister(sseUuid);
        }
    }

    public void sendErrorAndComplete(long userId, String sseUuid, String errorMsg) {
        SseEmitter sseEmitter = get(sseUuid);
        doSendErrorAndComplete(userId, sseUuid, sseEmitter, errorMsg);
    }

    // ==================== 静态方法（接受 uuid，通过 SpringUtil 获取 manager） ====================

    public static void parseAndSendPartialMsg(String uuid, String content) {
        parseAndSendPartialMsg(uuid, "", content);
    }

    public static void sendAudio(String uuid, Object content) {
        SseEntry entry = getEntry(uuid);
        if (entry == null) return;
        try {
            entry.emitter().send(SseEmitter.event().name(AdiConstant.SSEEventName.AUDIO).data(content));
        } catch (IOException e) {
            log.error("stream onNext error", e);
            throw new RuntimeException(e);
        }
    }

    public static void sendThinking(String uuid, String content) {
        SseEntry entry = getEntry(uuid);
        if (entry == null) return;
        try {
            entry.emitter().send(SseEmitter.event().name(AdiConstant.SSEEventName.THINKING).data(content));
        } catch (IOException e) {
            log.error("stream onNext error", e);
            throw new RuntimeException(e);
        }
    }

    public static void sendToolCall(String uuid, String toolName, long durationMs, boolean success) {
        SseEntry entry = getEntry(uuid);
        if (entry == null) {
            return;
        }
        try {
            String safeName = toolName != null ? toolName : "unknown";
            String data = JsonUtil.toJson(Map.of("toolName", safeName, "durationMs", durationMs, "success", success));
            entry.emitter().send(SseEmitter.event().name(AdiConstant.SSEEventName.TOOL_CALL).data(data));
        } catch (Exception e) {
            log.error("sendToolCall error", e);
            SpringUtil.getBean(SseManager.class).unregister(uuid);
        }
    }

    public static void parseAndSendPartialMsg(String uuid, String name, String content) {
        SseEntry entry = getEntry(uuid);
        if (entry == null) return;
        String[] lines = content.split("[\\r\\n]", -1);
        if (lines.length > 1) {
            sendPartial(uuid, name, entry, " " + lines[0]);
            for (int i = 1; i < lines.length; i++) {
                sendPartial(uuid, name, entry, "-_wrap_-");
                sendPartial(uuid, name, entry, " " + lines[i]);
            }
        } else {
            sendPartial(uuid, name, entry, " " + content);
        }
    }

    public static void sendPartial(String uuid, String name, String msg) {
        SseEntry entry = getEntry(uuid);
        if (entry == null) return;
        sendPartial(uuid, name, entry, msg);
    }

    /** 内部 sendPartial：复用上层已获取的 entry */
    private static void sendPartial(String uuid, String name, SseEntry entry, String msg) {
        try {
            if (StringUtils.isNotBlank(name)) {
                entry.emitter().send(SseEmitter.event().name(name).data(msg));
            } else {
                entry.emitter().send(msg);
            }
        } catch (IOException ioException) {
            log.error("stream onNext error", ioException);
            SpringUtil.getBean(SseManager.class).unregister(uuid);
            throw new RuntimeException(ioException);
        }
    }

    public static void errorAndShutdown(Throwable error, String uuid) {
        SseEntry entry = getEntry(uuid);
        if (entry == null) return;
        log.error("stream error", error);
        try {
            entry.emitter().send(SseEmitter.event().name(AdiConstant.SSEEventName.ERROR).data(error.getMessage()));
        } catch (IOException e) {
            log.error("sse error", e);
        } finally {
            SpringUtil.getBean(SseManager.class).unregister(uuid);
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
        //只在 tokenUsage 不为 null 时才缓存，避免 NPE | Only cache when tokenUsage is non-null to avoid NPE
        if (response.metadata() != null && response.metadata().tokenUsage() != null) {
            LLMTokenUtil.cacheTokenUsage(SpringUtil.getBean(StringRedisTemplate.class), uuid, response.metadata().tokenUsage());
        }

        PromptMeta questionMeta = new PromptMeta(inputTokenCount, uuid);
        AnswerMeta answerMeta = AnswerMeta.builder().tokens(outputTokenCount).uuid(UuidUtil.createShort()).build();
        return Pair.of(questionMeta, answerMeta);
    }

    public void deleteCache(String cache) {
        stringRedisTemplate.delete(cache);
    }

    // ==================== 内部方法 ====================

    /**
     * 通过 uuid 从管理器获取 SseEntry（单次查找，避免 TOCTOU）
     * <p>
     * Look up SseEntry by uuid from the manager (single lookup, avoids TOCTOU).
     * </p>
     */
    private static SseEntry getEntry(String uuid) {
        return SpringUtil.getBean(SseManager.class).entries.get(uuid);
    }

    /**
     * 发送错误并完成（内部实现，允许 sseEmitter 为 null）
     * <p>
     * Send error and complete (internal impl, allows null sseEmitter).
     * </p>
     */
    private void doSendErrorAndComplete(long userId, String sseUuid, SseEmitter sseEmitter, String errorMsg) {
        if (sseEmitter == null) {
            decActiveSseCount(userId, sseUuid);
            return;
        }
        SseEntry entry = entries.get(sseUuid);
        if (entry == null) {
            // Emitter not yet registered (e.g. rejected by checkOrComplete before startSse).
            // Send error directly so the frontend still gets a response.
            try {
                sseEmitter.send(SseEmitter.event().name(AdiConstant.SSEEventName.ERROR).data(Objects.toString(errorMsg, "")));
                sseEmitter.complete();
            } catch (IOException e) {
                log.warn("sendErrorAndComplete userId:{},errorMsg:{}", userId, errorMsg);
            }
            decActiveSseCount(userId, sseUuid);
            return;
        }
        try {
            entry.emitter().send(SseEmitter.event().name(AdiConstant.SSEEventName.ERROR).data(Objects.toString(errorMsg, "")));
            entry.emitter().complete();
        } catch (Exception e) {
            log.warn("sendErrorAndComplete userId:{},errorMsg:{}", userId, errorMsg, e);
        } finally {
            unregister(sseUuid);
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

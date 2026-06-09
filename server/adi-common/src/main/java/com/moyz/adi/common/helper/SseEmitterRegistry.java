package com.moyz.adi.common.helper;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.util.concurrent.ConcurrentHashMap;

/**
 * SseEmitter 注册中心
 * <p>
 * 管理 SSE 请求标识(uuid)与 SseEmitter 实例的映射关系，
 * 替代将 SseEmitter 在方法间层层传递的模式。
 * </p>
 * <p>
 * SseEmitter registry — manages the mapping between SSE request identifiers (uuid)
 * and SseEmitter instances, replacing the pattern of threading SseEmitter through method parameters.
 * </p>
 */
@Slf4j
@Component
public class SseEmitterRegistry {

    private final ConcurrentHashMap<String, SseEmitter> emitters = new ConcurrentHashMap<>();

    /**
     * 注册 SseEmitter
     *
     * @param uuid    SSE 请求标识 / SSE request identifier
     * @param emitter SseEmitter 实例 / SseEmitter instance
     */
    public void register(String uuid, SseEmitter emitter) {
        emitters.put(uuid, emitter);
    }

    /**
     * 获取 SseEmitter
     *
     * @param uuid SSE 请求标识 / SSE request identifier
     * @return SseEmitter 实例，不存在则返回 null / SseEmitter instance, or null if not found
     */
    public SseEmitter get(String uuid) {
        return emitters.get(uuid);
    }

    /**
     * 注销 SseEmitter
     *
     * @param uuid SSE 请求标识 / SSE request identifier
     */
    public void unregister(String uuid) {
        emitters.remove(uuid);
    }
}

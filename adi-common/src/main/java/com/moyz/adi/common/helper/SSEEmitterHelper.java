package com.moyz.adi.common.helper;

import com.moyz.adi.common.cosntant.RedisKeyConstant;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.interfaces.TriConsumer;
import com.moyz.adi.common.util.LocalCache;
import com.moyz.adi.common.vo.AnswerMeta;
import com.moyz.adi.common.vo.PromptMeta;
import com.moyz.adi.common.vo.SseAskParams;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.io.IOException;
import java.text.MessageFormat;
import java.util.concurrent.TimeUnit;

@Slf4j
@Service
public class SSEEmitterHelper {

    @Resource
    private StringRedisTemplate stringRedisTemplate;

    @Resource
    private RateLimitHelper rateLimitHelper;

    public void process(User user, SseAskParams sseAskParams, TriConsumer<String, PromptMeta, AnswerMeta> consumer) {
        SseEmitter sseEmitter = sseAskParams.getSseEmitter();

        //rate limit by system
        String requestTimesKey = MessageFormat.format(RedisKeyConstant.USER_REQUEST_TEXT_TIMES, user.getId());
        if (!rateLimitHelper.checkRequestTimes(requestTimesKey, LocalCache.TEXT_RATE_LIMIT_CONFIG)) {
            sendErrorMsg(sseEmitter, "访问太过频繁");
            return;
        }

        //Check: If still waiting response
        String askingKey = MessageFormat.format(RedisKeyConstant.USER_ASKING, user.getId());
        String askingVal = stringRedisTemplate.opsForValue().get(askingKey);
        if (StringUtils.isNotBlank(askingVal)) {
            sendErrorMsg(sseEmitter, "正在回复中...");
            return;
        }
        stringRedisTemplate.opsForValue().set(askingKey, "1", 15, TimeUnit.SECONDS);
        try {
            sseEmitter.send(SseEmitter.event().name("start"));
        } catch (IOException e) {
            log.error("error", e);
            sseEmitter.completeWithError(e);
            stringRedisTemplate.delete(askingKey);
            return;
        }

        rateLimitHelper.increaseRequestTimes(requestTimesKey, LocalCache.TEXT_RATE_LIMIT_CONFIG);

        sseEmitter.onCompletion(() -> {
            log.info("response complete,uid:{}", user.getId());
        });
        sseEmitter.onTimeout(() -> log.warn("sseEmitter timeout,uid:{},on timeout:{}", user.getId(), sseEmitter.getTimeout()));
        sseEmitter.onError(
                throwable -> {
                    try {
                        log.error("sseEmitter error,uid:{},on error:{}", user.getId(), throwable);
                        sseEmitter.send(SseEmitter.event().name("error").data(throwable.getMessage()));
                    } catch (IOException e) {
                        log.error("error", e);
                    } finally {
                        stringRedisTemplate.delete(askingKey);
                    }
                }
        );
        new LLMContext(sseAskParams.getModelName()).getLLMService().sseChat(sseAskParams, (response, promptMeta, answerMeta) -> {
            try {
                consumer.accept((String) response, (PromptMeta) promptMeta, (AnswerMeta) answerMeta);
            } catch (Exception e) {
                log.error("error:", e);
            } finally {
                stringRedisTemplate.delete(askingKey);
            }
        });
    }

    public void sendErrorMsg(SseEmitter sseEmitter, String errorMsg) {
        try {
            sseEmitter.send(SseEmitter.event().name("error").data(errorMsg));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        sseEmitter.complete();
    }
}

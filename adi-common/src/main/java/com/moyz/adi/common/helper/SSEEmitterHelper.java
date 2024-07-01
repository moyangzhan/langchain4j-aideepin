package com.moyz.adi.common.helper;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.cosntant.RedisKeyConstant;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.interfaces.TriConsumer;
import com.moyz.adi.common.util.LocalCache;
import com.moyz.adi.common.vo.AnswerMeta;
import com.moyz.adi.common.vo.PromptMeta;
import com.moyz.adi.common.vo.SseAskParams;
import dev.langchain4j.rag.content.retriever.ContentRetriever;
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

    public boolean checkOrComplete(User user, SseEmitter sseEmitter) {
        //Check: rate limit
        String requestTimesKey = MessageFormat.format(RedisKeyConstant.USER_REQUEST_TEXT_TIMES, user.getId());
        if (!rateLimitHelper.checkRequestTimes(requestTimesKey, LocalCache.TEXT_RATE_LIMIT_CONFIG)) {
            sendErrorAndComplete(user.getId(), sseEmitter, "访问太过频繁");
            return false;
        }

        //Check: If still waiting response
        String askingKey = MessageFormat.format(RedisKeyConstant.USER_ASKING, user.getId());
        String askingVal = stringRedisTemplate.opsForValue().get(askingKey);
        if (StringUtils.isNotBlank(askingVal)) {
            sendErrorAndComplete(user.getId(), sseEmitter, "正在回复中...");
            return false;
        }
        return true;
    }

    public void startSse(User user, SseEmitter sseEmitter) {

        String askingKey = MessageFormat.format(RedisKeyConstant.USER_ASKING, user.getId());
        stringRedisTemplate.opsForValue().set(askingKey, "1", 15, TimeUnit.SECONDS);

        String requestTimesKey = MessageFormat.format(RedisKeyConstant.USER_REQUEST_TEXT_TIMES, user.getId());
        rateLimitHelper.increaseRequestTimes(requestTimesKey, LocalCache.TEXT_RATE_LIMIT_CONFIG);
        try {
            sseEmitter.send(SseEmitter.event().name(AdiConstant.SSEEventName.START));
        } catch (IOException e) {
            log.error("startSse error", e);
            sseEmitter.completeWithError(e);
            stringRedisTemplate.delete(askingKey);
        }
    }

    /**
     * 普通提问处理
     *
     * @param user
     * @param sseAskParams
     * @param consumer
     */
    public void commonProcess(User user, SseAskParams sseAskParams, TriConsumer<String, PromptMeta, AnswerMeta> consumer) {
        String askingKey = registerSseEventCallBack(user, sseAskParams);
        new LLMContext(sseAskParams.getModelName()).getLLMService().commonChat(sseAskParams, (response, promptMeta, answerMeta) -> {
            try {
                consumer.accept((String) response, (PromptMeta) promptMeta, (AnswerMeta) answerMeta);
            } catch (Exception e) {
                log.error("commonProcess error", e);
            } finally {
                stringRedisTemplate.delete(askingKey);
            }
        });
    }

    /**
     * 使用RAG处理提问
     *
     * @param contentRetriever
     * @param user
     * @param sseAskParams
     * @param consumer
     */
    public void ragProcess(ContentRetriever contentRetriever, User user, SseAskParams sseAskParams, TriConsumer<String, PromptMeta, AnswerMeta> consumer) {
        String askingKey = registerSseEventCallBack(user, sseAskParams);
        new LLMContext(sseAskParams.getModelName()).getLLMService().ragChat(contentRetriever, sseAskParams, (response, promptMeta, answerMeta) -> {
            try {
                consumer.accept((String) response, (PromptMeta) promptMeta, (AnswerMeta) answerMeta);
            } catch (Exception e) {
                log.error("ragProcess error", e);
            } finally {
                stringRedisTemplate.delete(askingKey);
            }
        });
    }

    /**
     * 注册SSEEmiiter的回调
     *
     * @param user
     * @param sseAskParams
     * @return
     */
    private String registerSseEventCallBack(User user, SseAskParams sseAskParams) {
        String askingKey = MessageFormat.format(RedisKeyConstant.USER_ASKING, user.getId());
        SseEmitter sseEmitter = sseAskParams.getSseEmitter();
        sseEmitter.onCompletion(() -> log.info("response complete,uid:{}", user.getId()));
        sseEmitter.onTimeout(() -> log.warn("sseEmitter timeout,uid:{},on timeout:{}", user.getId(), sseEmitter.getTimeout()));
        sseEmitter.onError(
                throwable -> {
                    try {
                        log.error("sseEmitter error,uid:{},on error:{}", user.getId(), throwable);
                        sseEmitter.send(SseEmitter.event().name(AdiConstant.SSEEventName.ERROR).data(throwable.getMessage()));
                    } catch (IOException e) {
                        log.error("error", e);
                    } finally {
                        stringRedisTemplate.delete(askingKey);
                    }
                }
        );
        return askingKey;
    }

    public void sendAndComplete(long userId, SseEmitter sseEmitter, String msg) {
        try {
            sseEmitter.send(SseEmitter.event().name(AdiConstant.SSEEventName.START));
            sseEmitter.send(SseEmitter.event().name(AdiConstant.SSEEventName.DONE).data(msg));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        sseEmitter.complete();
        delSseRequesting(userId);
    }

    public void sendErrorAndComplete(long userId, SseEmitter sseEmitter, String errorMsg) {
        try {
            sseEmitter.send(SseEmitter.event().name(AdiConstant.SSEEventName.ERROR).data(errorMsg));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        sseEmitter.complete();
        delSseRequesting(userId);
    }

    private void delSseRequesting(long userId) {
        String askingKey = MessageFormat.format(RedisKeyConstant.USER_ASKING, userId);
        stringRedisTemplate.delete(askingKey);
    }
}

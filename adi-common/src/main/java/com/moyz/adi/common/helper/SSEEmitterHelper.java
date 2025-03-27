package com.moyz.adi.common.helper;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.cosntant.RedisKeyConstant;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.interfaces.TriConsumer;
import com.moyz.adi.common.util.*;
import com.moyz.adi.common.vo.AnswerMeta;
import com.moyz.adi.common.vo.ChatMeta;
import com.moyz.adi.common.vo.PromptMeta;
import com.moyz.adi.common.vo.SseAskParams;
import com.theokanning.openai.OpenAiError;
import dev.ai4j.openai4j.OpenAiHttpException;
import dev.langchain4j.model.chat.response.ChatResponse;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
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
        this.startSse(user, sseEmitter, null);
    }

    public void startSse(User user, SseEmitter sseEmitter, String data) {

        String askingKey = MessageFormat.format(RedisKeyConstant.USER_ASKING, user.getId());
        stringRedisTemplate.opsForValue().set(askingKey, "1", 15, TimeUnit.SECONDS);

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
            stringRedisTemplate.delete(askingKey);
        }
    }

    /**
     * event_stream请求，完成后关闭sse并执行回调
     *
     * @param sseAskParams 请求参数
     * @param shutdownSse  请求结束后是否关闭sse emitter
     * @param consumer     请求结束后的回调
     */
    public void call(SseAskParams sseAskParams, boolean shutdownSse, TriConsumer<String, PromptMeta, AnswerMeta> consumer) {
        String askingKey = registerEventStreamListener(sseAskParams);
        LLMContext.getLLMServiceByName(sseAskParams.getModelName()).streamingChat(sseAskParams, shutdownSse, (response, promptMeta, answerMeta) -> {
            try {
                consumer.accept(response, promptMeta, answerMeta);
            } catch (Exception e) {
                log.error("commonProcess error", e);
            } finally {
                stringRedisTemplate.delete(askingKey);
            }
        });
    }

    /**
     * 注册event stream的事件
     *
     * @param sseAskParams 参数
     * @return 用户请求标识
     */
    public String registerEventStreamListener(SseAskParams sseAskParams) {
        User user = sseAskParams.getUser();
        String askingKey = MessageFormat.format(RedisKeyConstant.USER_ASKING, user.getId());
        SseEmitter sseEmitter = sseAskParams.getSseEmitter();
        sseEmitter.onCompletion(() -> log.info("response complete,uid:{}", user.getId()));
        sseEmitter.onTimeout(() -> log.warn("sseEmitter timeout,uid:{},on timeout:{}", user.getId(), sseEmitter.getTimeout()));
        sseEmitter.onError(
                throwable -> {
                    try {
                        log.error("sseEmitter error,uid:{},on error", user.getId(), throwable);
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

    public void sendComplete(long userId, SseEmitter sseEmitter, String msg) {
        try {
            sseEmitter.send(SseEmitter.event().name(AdiConstant.SSEEventName.DONE).data(msg));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        sseEmitter.complete();
        delSseRequesting(userId);
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
            sseEmitter.send(SseEmitter.event().name(AdiConstant.SSEEventName.ERROR).data(StringUtils.defaultString(errorMsg, "")));
        } catch (IOException e) {
            log.warn("sendErrorAndComplete userId:{},errorMsg:{}", userId, errorMsg);
            throw new RuntimeException(e);
        }
        sseEmitter.complete();
        delSseRequesting(userId);
    }

    private void delSseRequesting(long userId) {
        String askingKey = MessageFormat.format(RedisKeyConstant.USER_ASKING, userId);
        stringRedisTemplate.delete(askingKey);
    }

    public static void parseAndSendPartialMsg(SseEmitter sseEmitter, String content) {
        parseAndSendPartialMsg(sseEmitter, "", content);
    }

    public static void parseAndSendPartialMsg(SseEmitter sseEmitter, String name, String content) {
        try {
            String[] lines = content.split("[\\r\\n]", -1);
            if (lines.length > 1) {
                sendPartial(sseEmitter, name, " " + lines[0]);
                for (int i = 1; i < lines.length; i++) {
                    sendPartial(sseEmitter, name, "-_wrap_-");
                    sendPartial(sseEmitter, name, " " + lines[i]);
                }
            } else {
                sendPartial(sseEmitter, name, " " + content);
            }
//            content = content.replaceAll("[\\r\\n]", "\ndata:");
//            sendPartial(sseEmitter, name, " " + content);
        } catch (IOException e) {
            log.error("stream onNext error", e);
        }
    }

    private static void sendPartial(SseEmitter sseEmitter, String name, String msg) throws IOException {
        if (StringUtils.isNotBlank(name)) {
            sseEmitter.send(SseEmitter.event().name(name).data(msg));
        } else {
            sseEmitter.send(msg);
        }
    }

    /**
     * 计算llm返回消费的token并关闭sse，执行回调
     *
     * @param response    llm返回的最终内容
     * @param sseEmitter  sse emitter
     * @param uuid        标识
     * @param shutdownSse 是否关闭sse
     * @return 请求及答案消息对
     */
    public static Pair<PromptMeta, AnswerMeta> calculateTokenAndShutdown(ChatResponse response, SseEmitter sseEmitter, String uuid, boolean shutdownSse) {
        log.info("返回数据结束了:{}", response);
        //缓存以便后续统计此次提问的消耗总token
        int inputTokenCount = response.metadata().tokenUsage().totalTokenCount();
        int outputTokenCount = response.metadata().tokenUsage().outputTokenCount();
        log.info("StreamingChatLanguageModel token cost,uuid:{},inputTokenCount:{},outputTokenCount:{}", uuid, inputTokenCount, outputTokenCount);
        LLMTokenUtil.cacheTokenUsage(SpringUtil.getBean(StringRedisTemplate.class), uuid, response.metadata().tokenUsage());

        PromptMeta questionMeta = new PromptMeta(inputTokenCount, uuid);
        AnswerMeta answerMeta = new AnswerMeta(outputTokenCount, UuidUtil.createShort());
        ChatMeta chatMeta = new ChatMeta(questionMeta, answerMeta);
        String meta = JsonUtil.toJson(chatMeta).replace("\r\n", "");
        log.info("meta:" + meta);
        try {
            sseEmitter.send(SseEmitter.event().name(AdiConstant.SSEEventName.DONE).data(" " + AdiConstant.SSEEventName.META + meta));
        } catch (IOException e) {
            log.error("stream onComplete error", e);
            throw new RuntimeException(e);
        }
        // close eventSourceEmitter after tokens was calculated
        if (shutdownSse) {
            sseEmitter.complete();
        }
        return Pair.of(questionMeta, answerMeta);
    }

    public static void errorAndShutdown(Throwable error, SseEmitter sseEmitter) {
        log.error("stream error", error);
        try {
            String errorMsg = error.getMessage();
            if (error instanceof OpenAiHttpException openAiHttpException) {
                OpenAiError openAiError = JsonUtil.fromJson(openAiHttpException.getMessage(), OpenAiError.class);
                if (null != openAiError) {
                    errorMsg = openAiError.getError().getMessage();
                }
            }
            sseEmitter.send(SseEmitter.event().name(AdiConstant.SSEEventName.ERROR).data(errorMsg));
        } catch (IOException e) {
            log.error("sse error", e);
        }
        sseEmitter.complete();
    }
}

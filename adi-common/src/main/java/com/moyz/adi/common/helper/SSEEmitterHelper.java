package com.moyz.adi.common.helper;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.cosntant.RedisKeyConstant;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.interfaces.TriConsumer;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.util.LocalCache;
import com.moyz.adi.common.util.SpringUtil;
import com.moyz.adi.common.util.UuidUtil;
import com.moyz.adi.common.vo.AnswerMeta;
import com.moyz.adi.common.vo.ChatMeta;
import com.moyz.adi.common.vo.PromptMeta;
import com.moyz.adi.common.vo.SseAskParams;
import com.theokanning.openai.OpenAiError;
import dev.ai4j.openai4j.OpenAiHttpException;
import dev.langchain4j.service.TokenStream;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.io.IOException;
import java.text.MessageFormat;
import java.util.concurrent.TimeUnit;

import static com.moyz.adi.common.cosntant.RedisKeyConstant.TOKEN_USAGE_KEY;

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
     * @param sseAskParams
     * @param consumer
     */
    public void commonProcess(SseAskParams sseAskParams, TriConsumer<String, PromptMeta, AnswerMeta> consumer) {
        String askingKey = registerSseEventCallBack(sseAskParams);
        LLMContext.getLLMServiceByName(sseAskParams.getModelName()).commonChat(sseAskParams, (response, promptMeta, answerMeta) -> {
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
     * 注册SSEEmiiter的回调
     *
     * @param sseAskParams
     * @return
     */
    public String registerSseEventCallBack(SseAskParams sseAskParams) {
        User user = sseAskParams.getUser();
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

    /**
     * 注册TokenStream的回调
     *
     * @param tokenStream
     * @param params
     * @param consumer
     */
    public static void registerTokenStreamCallBack(TokenStream tokenStream, SseAskParams params, TriConsumer<String, PromptMeta, AnswerMeta> consumer) {
        tokenStream
                .onNext((content) -> {
                    log.info("get content:{}", content);
                    //加空格配合前端的fetchEventSource进行解析，见https://github.com/Azure/fetch-event-source/blob/45ac3cfffd30b05b79fbf95c21e67d4ef59aa56a/src/parse.ts#L129-L133
                    try {
                        String[] lines = content.split("[\\r\\n]", -1);
                        if (lines.length > 1) {
                            params.getSseEmitter().send(" " + lines[0]);
                            for (int i = 1; i < lines.length; i++) {
                                /**
                                 * 当响应结果的content中包含有多行文本时，
                                 * 前端的fetch-event-source框架的BUG会将包含有换行符的那一行内容替换为空字符串，
                                 * 故需要先将换行符与后面的内容拆分并转成，前端碰到换行标志时转成换行符处理
                                 */
                                params.getSseEmitter().send("-_-_wrap_-_-");
                                params.getSseEmitter().send(" " + lines[i]);
                            }
                        } else {
                            params.getSseEmitter().send(" " + content);
                        }
                    } catch (IOException e) {
                        log.error("stream onNext error", e);
                    }
                })
                .onComplete((response) -> {
                    log.info("返回数据结束了:{}", response);
                    //缓存以便后续统计此次提问的消耗总token
                    int inputTokenCount = response.tokenUsage().totalTokenCount();
                    int outputTokenCount = response.tokenUsage().outputTokenCount();
                    log.info("StreamingChatLanguageModel token cost,uuid:{},inputTokenCount:{},outputTokenCount:{}", params.getUuid(), inputTokenCount, outputTokenCount);
                    SpringUtil.getBean(StringRedisTemplate.class).opsForList().rightPushAll(MessageFormat.format(TOKEN_USAGE_KEY, params.getUuid()), String.valueOf(inputTokenCount), String.valueOf(outputTokenCount));

                    PromptMeta questionMeta = new PromptMeta(inputTokenCount, params.getUuid());
                    AnswerMeta answerMeta = new AnswerMeta(outputTokenCount, UuidUtil.createShort());
                    ChatMeta chatMeta = new ChatMeta(questionMeta, answerMeta);
                    String meta = JsonUtil.toJson(chatMeta).replace("\r\n", "");
                    log.info("meta:" + meta);
                    try {
                        params.getSseEmitter().send(SseEmitter.event().name(AdiConstant.SSEEventName.DONE).data(" " + AdiConstant.SSEEventName.META + meta));
                    } catch (IOException e) {
                        log.error("stream onComplete error", e);
                        throw new RuntimeException(e);
                    }
                    // close eventSourceEmitter after tokens was calculated
                    params.getSseEmitter().complete();
                    consumer.accept(response.content().text(), questionMeta, answerMeta);
                })
                .onError((error) -> {
                    log.error("stream error", error);
                    try {
                        String errorMsg = error.getMessage();
                        if (error instanceof OpenAiHttpException) {
                            OpenAiHttpException openAiHttpException = (OpenAiHttpException) error;
                            OpenAiError openAiError = JsonUtil.fromJson(openAiHttpException.getMessage(), OpenAiError.class);
                            errorMsg = openAiError.getError().getMessage();
                        }
                        params.getSseEmitter().send(SseEmitter.event().name(AdiConstant.SSEEventName.ERROR).data(errorMsg));
                    } catch (IOException e) {
                        log.error("sse error", e);
                    }
                    params.getSseEmitter().complete();
                })
                .start();
    }
}

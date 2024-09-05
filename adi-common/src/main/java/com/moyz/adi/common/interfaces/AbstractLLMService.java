package com.moyz.adi.common.interfaces;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.util.*;
import com.moyz.adi.common.vo.*;
import dev.langchain4j.memory.chat.ChatMemoryProvider;
import dev.langchain4j.memory.chat.MessageWindowChatMemory;
import dev.langchain4j.model.chat.ChatLanguageModel;
import dev.langchain4j.model.chat.StreamingChatLanguageModel;
import dev.langchain4j.rag.RetrievalAugmentor;
import dev.langchain4j.rag.content.retriever.ContentRetriever;
import dev.langchain4j.rag.query.transformer.CompressingQueryTransformer;
import dev.langchain4j.rag.query.transformer.QueryTransformer;
import dev.langchain4j.service.AiServices;
import dev.langchain4j.service.TokenStream;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.io.IOException;
import java.net.Proxy;
import java.text.MessageFormat;
import java.time.Duration;

import static com.moyz.adi.common.cosntant.AdiConstant.LLM_MAX_INPUT_TOKENS_DEFAULT;
import static com.moyz.adi.common.cosntant.RedisKeyConstant.TOKEN_USAGE_KEY;
import static com.moyz.adi.common.enums.ErrorEnum.B_LLM_SERVICE_DISABLED;

@Slf4j
public abstract class AbstractLLMService<T> {

    protected Proxy proxy;
    protected AiModel aiModel;
    protected T modelPlatformSetting;

    protected StringRedisTemplate stringRedisTemplate;

    protected AbstractLLMService(AiModel aiModel, String settingName, Class<T> clazz) {
        this.aiModel = aiModel;
        String st = LocalCache.CONFIGS.get(settingName);
        modelPlatformSetting = JsonUtil.fromJson(st, clazz);

        initMaxInputTokens();
    }

    private void initMaxInputTokens() {
        if (this.aiModel.getMaxInputTokens() < 1) {
            this.aiModel.setMaxInputTokens(LLM_MAX_INPUT_TOKENS_DEFAULT);
        }
    }

    private StringRedisTemplate getStringRedisTemplate() {
        if (null == this.stringRedisTemplate) {
            this.stringRedisTemplate = SpringUtil.getBean(StringRedisTemplate.class);
        }
        return this.stringRedisTemplate;
    }

    public AbstractLLMService setProxy(Proxy proxy) {
        this.proxy = proxy;
        return this;
    }

    /**
     * 检测该service是否可用（不可用的情况通常是没有配置key）
     *
     * @return
     */
    public abstract boolean isEnabled();

    public ChatLanguageModel buildChatLLM(LLMBuilderProperties properties, String uuid) {
        return new AdiChatLanguageModelImpl(doBuildChatLLM(properties), response -> {
            String redisKey = MessageFormat.format(TOKEN_USAGE_KEY, uuid);
            getStringRedisTemplate().expire(redisKey, Duration.ofMinutes(10));
            int inputTokenCount = response.tokenUsage().inputTokenCount();
            int outputTokenCount = response.tokenUsage().outputTokenCount();
            getStringRedisTemplate().opsForList().rightPushAll(redisKey, String.valueOf(inputTokenCount), String.valueOf(outputTokenCount));
        });
    }

    protected abstract ChatLanguageModel doBuildChatLLM(LLMBuilderProperties properties);

    protected abstract StreamingChatLanguageModel buildStreamingChatLLM(LLMBuilderProperties properties);

    protected abstract String parseError(Object error);

    /**
     * RAG请求，对prompt进行各种增强后发给AI
     * ps: 挂载了知识库的请求才进行RAG增强
     *
     * @param contentRetriever
     * @param params
     * @param consumer
     */
    public void ragChat(ContentRetriever contentRetriever, SseAskParams params, TriConsumer<String, PromptMeta, AnswerMeta> consumer) {
        if (!isEnabled()) {
            log.error("llm service is disabled");
            throw new BaseException(B_LLM_SERVICE_DISABLED);
        }
        TokenStream tokenStream;
        AssistantChatParams assistantChatParams = params.getAssistantChatParams();
        if (StringUtils.isNotBlank(assistantChatParams.getMessageId())) {
            ChatMemoryProvider chatMemoryProvider = memoryId -> MessageWindowChatMemory.builder()
                    .id(memoryId)
                    .maxMessages(2)
                    .chatMemoryStore(MapDBChatMemoryStore.getSingleton())
                    .build();
            QueryTransformer queryTransformer = new CompressingQueryTransformer(buildChatLLM(params.getLlmBuilderProperties(), params.getUuid()));
            RetrievalAugmentor retrievalAugmentor = AdiKnowledgeBaseRetrievalAugmentor.builder()
                    .queryTransformer(queryTransformer)
                    .contentRetriever(contentRetriever)
                    .maxInputTokens(aiModel.getMaxInputTokens())
                    .inputAdaptorMsgConsumer(inputAdaptorMsg -> {
                        log.info(inputAdaptorMsg.toString());
                        if (inputAdaptorMsg.getTokenTooMuch() == InputAdaptorMsg.TOKEN_TOO_MUCH_QUESTION) {
                            //TODO 提示用户问题过长
                        }
                    })
                    .build();
            IChatAssistant assistant = AdiAiServices.builder(IChatAssistant.class, aiModel.getMaxInputTokens())
                    .streamingChatLanguageModel(buildStreamingChatLLM(params.getLlmBuilderProperties()))
                    .retrievalAugmentor(retrievalAugmentor)
                    .chatMemoryProvider(chatMemoryProvider)
                    .build();
            if (StringUtils.isNotBlank(assistantChatParams.getSystemMessage())) {
                tokenStream = assistant.chatWith(assistantChatParams.getMessageId(), assistantChatParams.getSystemMessage(), assistantChatParams.getUserMessage());
            } else {
                tokenStream = assistant.chatWithMemory(assistantChatParams.getMessageId(), assistantChatParams.getUserMessage());
            }
        } else {
            IChatAssistant assistant = AiServices.builder(IChatAssistant.class)
                    .streamingChatLanguageModel(buildStreamingChatLLM(params.getLlmBuilderProperties()))
                    .contentRetriever(contentRetriever)
                    .build();
            if (StringUtils.isNotBlank(assistantChatParams.getSystemMessage())) {
                tokenStream = assistant.chatWithSystem(assistantChatParams.getSystemMessage(), assistantChatParams.getUserMessage());
            } else {
                tokenStream = assistant.chatSimple(assistantChatParams.getUserMessage());
            }
        }
        registerTokenStreamCallBack(tokenStream, params, consumer);
    }

    /**
     * 普通聊天，将原始的用户问题及历史消息发送给AI
     *
     * @param params
     * @param consumer
     */
    public void commonChat(SseAskParams params, TriConsumer<String, PromptMeta, AnswerMeta> consumer) {
        if (!isEnabled()) {
            log.error("llm service is disabled");
            throw new BaseException(B_LLM_SERVICE_DISABLED);
        }

        AssistantChatParams assistantChatParams = params.getAssistantChatParams();
        log.info("sseChat,messageId:{}", assistantChatParams.getMessageId());

        TokenStream tokenStream;
        //chat with memory
        if (StringUtils.isNotBlank(assistantChatParams.getMessageId())) {
            ChatMemoryProvider chatMemoryProvider = memoryId -> MessageWindowChatMemory.builder()
                    .id(memoryId)
                    .maxMessages(6)
                    .chatMemoryStore(MapDBChatMemoryStore.getSingleton())
                    .build();
            IChatAssistant assistant = AdiAiServices.builder(IChatAssistant.class, aiModel.getMaxInputTokens())
                    .streamingChatLanguageModel(buildStreamingChatLLM(params.getLlmBuilderProperties()))
                    .chatMemoryProvider(chatMemoryProvider)
                    .build();
            if (StringUtils.isNotBlank(assistantChatParams.getSystemMessage())) {
                tokenStream = assistant.chatWith(assistantChatParams.getMessageId(), assistantChatParams.getSystemMessage(), assistantChatParams.getUserMessage());
            } else {
                tokenStream = assistant.chatWithMemory(assistantChatParams.getMessageId(), assistantChatParams.getUserMessage());
            }
        }
        //chat without memory
        else {
            IChatAssistant assistant = AiServices.builder(IChatAssistant.class)
                    .streamingChatLanguageModel(buildStreamingChatLLM(params.getLlmBuilderProperties()))
                    .build();
            if (StringUtils.isNotBlank(assistantChatParams.getSystemMessage())) {
                tokenStream = assistant.chatWithSystem(assistantChatParams.getSystemMessage(), assistantChatParams.getUserMessage());
            } else {
                tokenStream = assistant.chatSimple(assistantChatParams.getUserMessage());
            }
        }
        registerTokenStreamCallBack(tokenStream, params, consumer);
    }

    /**
     * 注册TokenStream的回调
     *
     * @param tokenStream
     * @param params
     * @param consumer
     */
    private void registerTokenStreamCallBack(TokenStream tokenStream, SseAskParams params, TriConsumer<String, PromptMeta, AnswerMeta> consumer) {
        tokenStream
                .onNext((content) -> {
                    log.info("get content:{}", content);
                    //加空格配合前端的fetchEventSource进行解析，见https://github.com/Azure/fetch-event-source/blob/45ac3cfffd30b05b79fbf95c21e67d4ef59aa56a/src/parse.ts#L129-L133
                    try {
//                        String parsedContent = content;
//                        if (content.matches("(?s)\\r?\\n")) {
//                            log.info("parsedContent");
//                            parsedContent = parsedContent.replaceAll("(?m)(\\r?\\n)", "$0__");
//                        }
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
                    log.info("消耗的token,input:{},output:{}", inputTokenCount, outputTokenCount);
                    getStringRedisTemplate().opsForList().rightPushAll(MessageFormat.format(TOKEN_USAGE_KEY, params.getUuid()), String.valueOf(inputTokenCount), String.valueOf(outputTokenCount));

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
                        String errorMsg = parseError(error);
                        if (StringUtils.isBlank(errorMsg)) {
                            errorMsg = error.getMessage();
                        }
                        params.getSseEmitter().send(SseEmitter.event().name(AdiConstant.SSEEventName.ERROR).data(errorMsg));
                    } catch (IOException e) {
                        log.error("sse error", e);
                    }
                    params.getSseEmitter().complete();
                })
                .start();
    }

    public AiModel getAiModel() {
        return aiModel;
    }
}

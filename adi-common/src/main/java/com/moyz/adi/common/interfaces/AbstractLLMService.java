package com.moyz.adi.common.interfaces;

import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.helper.SSEEmitterHelper;
import com.moyz.adi.common.rag.AdiAiServices;
import com.moyz.adi.common.rag.AdiChatLanguageModelImpl;
import com.moyz.adi.common.util.*;
import com.moyz.adi.common.vo.*;
import dev.langchain4j.data.message.ImageContent;
import dev.langchain4j.memory.chat.ChatMemoryProvider;
import dev.langchain4j.memory.chat.MessageWindowChatMemory;
import dev.langchain4j.model.Tokenizer;
import dev.langchain4j.model.chat.ChatLanguageModel;
import dev.langchain4j.model.chat.StreamingChatLanguageModel;
import dev.langchain4j.service.TokenStream;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.data.redis.core.StringRedisTemplate;

import java.net.Proxy;
import java.util.List;

import static com.moyz.adi.common.cosntant.AdiConstant.LLM_MAX_INPUT_TOKENS_DEFAULT;
import static com.moyz.adi.common.enums.ErrorEnum.A_PARAMS_ERROR;
import static com.moyz.adi.common.enums.ErrorEnum.B_LLM_SERVICE_DISABLED;

@Slf4j
public abstract class AbstractLLMService<T> {

    protected Proxy proxy;
    @Getter
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

    public StringRedisTemplate getStringRedisTemplate() {
        if (null == this.stringRedisTemplate) {
            this.stringRedisTemplate = SpringUtil.getBean(StringRedisTemplate.class);
        }
        return this.stringRedisTemplate;
    }

    public AbstractLLMService<T> setProxy(Proxy proxy) {
        this.proxy = proxy;
        return this;
    }

    /**
     * 检测该service是否可用（不可用的情况通常是没有配置key）
     *
     * @return
     */
    public abstract boolean isEnabled();

    protected boolean checkBeforeChat(SseAskParams params) {
        return true;
    }

    public ChatLanguageModel buildChatLLM(LLMBuilderProperties properties, String uuid) {
        LLMBuilderProperties tmpProperties = properties;
        if (null == properties) {
            tmpProperties = new LLMBuilderProperties();
            tmpProperties.setTemperature(0.7);
            log.info("llmBuilderProperties is null, set default temperature:{}", tmpProperties.getTemperature());
        }
        if (null == tmpProperties.getTemperature() || tmpProperties.getTemperature() <= 0 || tmpProperties.getTemperature() > 1) {
            tmpProperties.setTemperature(0.7);
            log.info("llmBuilderProperties temperature is invalid, set default temperature:{}", tmpProperties.getTemperature());
        }
        return new AdiChatLanguageModelImpl(doBuildChatLLM(tmpProperties), response -> {
            int inputTokenCount = response.metadata().tokenUsage().inputTokenCount();
            int outputTokenCount = response.metadata().tokenUsage().outputTokenCount();
            log.info("ChatLanguageModel token cost,uuid:{},inputTokenCount:{},outputTokenCount:{}", uuid, inputTokenCount, outputTokenCount);
            LLMTokenUtil.cacheTokenUsage(getStringRedisTemplate(), uuid, response.metadata().tokenUsage());
        });
    }

    protected abstract ChatLanguageModel doBuildChatLLM(LLMBuilderProperties properties);

    public abstract StreamingChatLanguageModel buildStreamingChatLLM(LLMBuilderProperties properties);

    protected abstract LLMException parseError(Object error);

    public abstract Tokenizer getTokenEstimator();

    /**
     * 普通聊天，将原始的用户问题及历史消息发送给AI
     *
     * @param params   请求参数
     * @param consumer 响应结果回调
     */
    public void streamingChat(SseAskParams params, boolean shutdownSse, TriConsumer<String, PromptMeta, AnswerMeta> consumer) {
        TokenStream tokenStream = createTokenStream(params);
        tokenStream
                .onPartialResponse(content -> SSEEmitterHelper.parseAndSendPartialMsg(params.getSseEmitter(), content))
                .onCompleteResponse(response -> {
                    Pair<PromptMeta, AnswerMeta> pair = SSEEmitterHelper.calculateTokenAndShutdown(response, params.getSseEmitter(), params.getUuid(), shutdownSse);
                    consumer.accept(response.aiMessage().text(), pair.getLeft(), pair.getRight());
                })
                .onError(error -> SSEEmitterHelper.errorAndShutdown(error, params.getSseEmitter()))
                .start();
    }

    public String chat(SseAskParams params) {
        if (!isEnabled()) {
            log.error("llm service is disabled");
            throw new BaseException(B_LLM_SERVICE_DISABLED);
        }
        if (!checkBeforeChat(params)) {
            log.error("对话参数校验不通过");
            throw new BaseException(A_PARAMS_ERROR);
        }
        List<ImageContent> imageContents = ImageUtil.urlsToImageContent(params.getAssistantChatParams().getImageUrls());
        AssistantChatParams assistantChatParams = params.getAssistantChatParams();
        log.info("sseChat,messageId:{}", assistantChatParams.getMessageId());

        String response;
        //chat with memory
        if (StringUtils.isNotBlank(assistantChatParams.getMessageId())) {
            ChatMemoryProvider chatMemoryProvider = memoryId -> MessageWindowChatMemory.builder()
                    .id(memoryId)
                    .maxMessages(6)
                    .chatMemoryStore(MapDBChatMemoryStore.getSingleton())
                    .build();
            IChatAssistant assistant = AdiAiServices.builder(IChatAssistant.class, aiModel.getMaxInputTokens())
                    .chatLanguageModel(buildChatLLM(params.getLlmBuilderProperties(), params.getUuid()))
                    .chatMemoryProvider(chatMemoryProvider)
                    .build();
            if (StringUtils.isNotBlank(assistantChatParams.getSystemMessage())) {
                response = assistant.chatWithSystem(assistantChatParams.getMessageId(), assistantChatParams.getSystemMessage(), assistantChatParams.getUserMessage(), imageContents);
            } else {
                response = assistant.chat(assistantChatParams.getMessageId(), assistantChatParams.getUserMessage(), imageContents);
            }
        }
        //chat without memory
        else {
            ITempChatAssistant assistant = AdiAiServices.builder(ITempChatAssistant.class, aiModel.getMaxInputTokens())
                    .chatLanguageModel(buildChatLLM(params.getLlmBuilderProperties(), params.getUuid()))
                    .build();
            if (StringUtils.isNotBlank(assistantChatParams.getSystemMessage())) {
                response = assistant.chatWithSystem(assistantChatParams.getSystemMessage(), assistantChatParams.getUserMessage(), imageContents);
            } else {
                response = assistant.chatSimple(assistantChatParams.getUserMessage(), imageContents);
            }
        }
        return response;
    }

    private TokenStream createTokenStream(SseAskParams params) {
        if (!isEnabled()) {
            log.error("llm service is disabled");
            throw new BaseException(B_LLM_SERVICE_DISABLED);
        }
        if (!checkBeforeChat(params)) {
            log.error("对话参数校验不通过");
            throw new BaseException(A_PARAMS_ERROR);
        }
        List<ImageContent> imageContents = ImageUtil.urlsToImageContent(params.getAssistantChatParams().getImageUrls());
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
            IStreamingChatAssistant assistant = AdiAiServices.builder(IStreamingChatAssistant.class, aiModel.getMaxInputTokens())
                    .streamingChatLanguageModel(buildStreamingChatLLM(params.getLlmBuilderProperties()))
                    .chatMemoryProvider(chatMemoryProvider)
                    .build();
            if (StringUtils.isNotBlank(assistantChatParams.getSystemMessage())) {
                tokenStream = assistant.chatWithSystem(assistantChatParams.getMessageId(), assistantChatParams.getSystemMessage(), assistantChatParams.getUserMessage(), imageContents);
            } else {
                tokenStream = assistant.chat(assistantChatParams.getMessageId(), assistantChatParams.getUserMessage(), imageContents);
            }
        }
        //chat without memory
        else {
            ITempStreamingChatAssistant assistant = AdiAiServices.builder(ITempStreamingChatAssistant.class, aiModel.getMaxInputTokens())
                    .streamingChatLanguageModel(buildStreamingChatLLM(params.getLlmBuilderProperties()))
                    .build();
            if (StringUtils.isNotBlank(assistantChatParams.getSystemMessage())) {
                tokenStream = assistant.chatWithSystem(assistantChatParams.getSystemMessage(), assistantChatParams.getUserMessage(), imageContents);
            } else {
                tokenStream = assistant.chatSimple(assistantChatParams.getUserMessage(), imageContents);
            }
        }
        return tokenStream;
    }
}

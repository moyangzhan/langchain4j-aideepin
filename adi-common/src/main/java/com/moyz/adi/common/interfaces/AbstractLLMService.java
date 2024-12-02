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
import dev.langchain4j.model.chat.ChatLanguageModel;
import dev.langchain4j.model.chat.StreamingChatLanguageModel;
import dev.langchain4j.service.AiServices;
import dev.langchain4j.service.TokenStream;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.redis.core.StringRedisTemplate;

import java.net.Proxy;
import java.text.MessageFormat;
import java.time.Duration;
import java.util.List;

import static com.moyz.adi.common.cosntant.AdiConstant.LLM_MAX_INPUT_TOKENS_DEFAULT;
import static com.moyz.adi.common.cosntant.RedisKeyConstant.TOKEN_USAGE_KEY;
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

    private StringRedisTemplate getStringRedisTemplate() {
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
        return new AdiChatLanguageModelImpl(doBuildChatLLM(properties), response -> {
            String redisKey = MessageFormat.format(TOKEN_USAGE_KEY, uuid);
            getStringRedisTemplate().expire(redisKey, Duration.ofMinutes(10));
            int inputTokenCount = response.tokenUsage().inputTokenCount();
            int outputTokenCount = response.tokenUsage().outputTokenCount();
            log.info("ChatLanguageModel token cost,uuid:{},inputTokenCount:{},outputTokenCount:{}", uuid, inputTokenCount, outputTokenCount);
            getStringRedisTemplate().opsForList().rightPushAll(redisKey, String.valueOf(inputTokenCount), String.valueOf(outputTokenCount));
        });
    }

    protected abstract ChatLanguageModel doBuildChatLLM(LLMBuilderProperties properties);

    public abstract StreamingChatLanguageModel buildStreamingChatLLM(LLMBuilderProperties properties);

    protected abstract LLMException parseError(Object error);

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
            IChatAssistant assistant = AdiAiServices.builder(IChatAssistant.class, aiModel.getMaxInputTokens())
                    .streamingChatLanguageModel(buildStreamingChatLLM(params.getLlmBuilderProperties()))
                    .chatMemoryProvider(chatMemoryProvider)
                    .build();
            if (StringUtils.isNotBlank(assistantChatParams.getSystemMessage())) {
                tokenStream = assistant.chatWith(assistantChatParams.getMessageId(), assistantChatParams.getSystemMessage(), assistantChatParams.getUserMessage(), imageContents);
            } else {
                tokenStream = assistant.chatWithMemory(assistantChatParams.getMessageId(), assistantChatParams.getUserMessage(), imageContents);
            }
        }
        //chat without memory
        else {
            IChatAssistant assistant = AdiAiServices.builder(IChatAssistant.class, aiModel.getMaxInputTokens())
                    .streamingChatLanguageModel(buildStreamingChatLLM(params.getLlmBuilderProperties()))
                    .build();
            if (StringUtils.isNotBlank(assistantChatParams.getSystemMessage())) {
                tokenStream = assistant.chatWithSystem(assistantChatParams.getSystemMessage(), assistantChatParams.getUserMessage(), imageContents);
            } else {
                tokenStream = assistant.chatSimple(assistantChatParams.getUserMessage(), imageContents);
            }
        }
        SSEEmitterHelper.registerTokenStreamCallBack(tokenStream, params, consumer);
    }

}

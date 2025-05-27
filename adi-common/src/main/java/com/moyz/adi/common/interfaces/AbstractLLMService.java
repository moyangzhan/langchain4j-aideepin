package com.moyz.adi.common.interfaces;

import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.helper.SSEEmitterHelper;
import com.moyz.adi.common.rag.TokenEstimatorFactory;
import com.moyz.adi.common.rag.TokenEstimatorThreadLocal;
import com.moyz.adi.common.util.*;
import com.moyz.adi.common.vo.*;
import dev.langchain4j.data.message.*;
import dev.langchain4j.memory.chat.TokenWindowChatMemory;
import dev.langchain4j.model.TokenCountEstimator;
import dev.langchain4j.model.chat.ChatModel;
import dev.langchain4j.model.chat.StreamingChatModel;
import dev.langchain4j.model.chat.response.ChatResponse;
import dev.langchain4j.model.chat.response.StreamingChatResponseHandler;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.data.redis.core.StringRedisTemplate;

import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.List;

import static com.moyz.adi.common.cosntant.AdiConstant.LLM_MAX_INPUT_TOKENS_DEFAULT;
import static com.moyz.adi.common.enums.ErrorEnum.A_PARAMS_ERROR;
import static com.moyz.adi.common.enums.ErrorEnum.B_LLM_SERVICE_DISABLED;

@Slf4j
public abstract class AbstractLLMService<T> {

    protected InetSocketAddress proxyAddress;
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

    public AbstractLLMService<T> setProxyAddress(InetSocketAddress proxyAddress) {
        this.proxyAddress = proxyAddress;
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

    public ChatModel buildChatLLM(LLMBuilderProperties properties, String uuid) {
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
        return doBuildChatLLM(tmpProperties);
    }

    protected abstract ChatModel doBuildChatLLM(LLMBuilderProperties properties);

    public abstract StreamingChatModel buildStreamingChatLLM(LLMBuilderProperties properties);

    protected abstract LLMException parseError(Object error);

    public abstract TokenCountEstimator getTokenEstimator();

    /**
     * 普通聊天，将原始的用户问题及历史消息发送给AI
     *
     * @param params   请求参数
     * @param consumer 响应结果回调
     */
    public void streamingChat(SseAskParams params, boolean shutdownSse, TriConsumer<String, PromptMeta, AnswerMeta> consumer) {
        createTokenStream(params, new StreamingChatResponseHandler() {
            @Override
            public void onPartialResponse(String partialResponse) {
                SSEEmitterHelper.parseAndSendPartialMsg(params.getSseEmitter(), partialResponse);
            }

            @Override
            public void onCompleteResponse(ChatResponse response) {
                Pair<PromptMeta, AnswerMeta> pair = SSEEmitterHelper.calculateTokenAndShutdown(response, params.getSseEmitter(), params.getUuid(), shutdownSse);
                consumer.accept(response.aiMessage().text(), pair.getLeft(), pair.getRight());
            }

            @Override
            public void onError(Throwable error) {
                SSEEmitterHelper.errorAndShutdown(error, params.getSseEmitter());
            }
        });
    }

    public ChatResponse chat(SseAskParams params) {
        if (!isEnabled()) {
            log.error("llm service is disabled");
            throw new BaseException(B_LLM_SERVICE_DISABLED);
        }
        if (!checkBeforeChat(params)) {
            log.error("对话参数校验不通过");
            throw new BaseException(A_PARAMS_ERROR);
        }
        ChatModel chatModel = buildChatLLM(params.getLlmBuilderProperties(), params.getUuid());
        List<ChatMessage> chatMessages = createChatMessages(params.getAssistantChatParams());
        ChatResponse chatResponse = chatModel.chat(chatMessages);

        // 计算token使用量
        int inputTokenCount = chatResponse.metadata().tokenUsage().inputTokenCount();
        int outputTokenCount = chatResponse.metadata().tokenUsage().outputTokenCount();
        log.info("ChatModel token cost,uuid:{},inputTokenCount:{},outputTokenCount:{}", params.getUuid(), inputTokenCount, outputTokenCount);
        LLMTokenUtil.cacheTokenUsage(getStringRedisTemplate(), params.getUuid(), chatResponse.metadata().tokenUsage());

        return chatResponse;
    }

    private void createTokenStream(SseAskParams params, StreamingChatResponseHandler handler) {
        if (!isEnabled()) {
            log.error("llm service is disabled");
            throw new BaseException(B_LLM_SERVICE_DISABLED);
        }
        if (!checkBeforeChat(params)) {
            log.error("对话参数校验不通过");
            throw new BaseException(A_PARAMS_ERROR);
        }
        AssistantChatParams assistantChatParams = params.getAssistantChatParams();
        log.info("sseChat,messageId:{}", assistantChatParams.getMemoryId());
        List<ChatMessage> chatMessages = createChatMessages(assistantChatParams);
        buildStreamingChatLLM(params.getLlmBuilderProperties()).chat(chatMessages, handler);
    }

    private List<ChatMessage> createChatMessages(AssistantChatParams assistantChatParams) {
        String memoryId = assistantChatParams.getMemoryId();
        List<Content> userContents = new ArrayList<>();
        userContents.add(TextContent.from(assistantChatParams.getUserMessage()));
        List<ChatMessage> chatMessages = new ArrayList<>();
        if (StringUtils.isNotBlank(memoryId)) {

            TokenCountEstimator tokenCountEstimator;
            String tokenEstimatorName = TokenEstimatorThreadLocal.getTokenEstimator();
            if (StringUtils.isBlank(tokenEstimatorName) && null != getTokenEstimator()) {
                tokenCountEstimator = getTokenEstimator();
            } else {
                tokenCountEstimator = TokenEstimatorFactory.create(tokenEstimatorName);
            }

            TokenWindowChatMemory memory = TokenWindowChatMemory.builder()
                    .chatMemoryStore(MapDBChatMemoryStore.getSingleton())
                    .id(memoryId)
                    .maxTokens(aiModel.getMaxInputTokens(), tokenCountEstimator)
                    .build();
            if (StringUtils.isNotBlank(assistantChatParams.getSystemMessage())) {
                memory.add(SystemMessage.from(assistantChatParams.getSystemMessage()));
            }
            memory.add(UserMessage.from(userContents));

            //截断文本消息后再追加图片消息
            chatMessages.addAll(memory.messages());

            //重新组装用户消息及图片消息到chatMessage
            List<Content> imageContents = ImageUtil.urlsToImageContent(assistantChatParams.getImageUrls());
            if (CollectionUtils.isNotEmpty(imageContents)) {
                int lastIndex = chatMessages.size() - 1;
                UserMessage lastMessage = (UserMessage) chatMessages.get(lastIndex);
                chatMessages.remove(lastIndex);
                List<Content> userMessage = new ArrayList<>();
                userMessage.addAll(lastMessage.contents());
                userMessage.addAll(ImageUtil.urlsToImageContent(assistantChatParams.getImageUrls()));
                chatMessages.add(UserMessage.from(userMessage));
            }
            return chatMessages;
        } else {
            if (StringUtils.isNotBlank(assistantChatParams.getSystemMessage())) {
                chatMessages.add(SystemMessage.from(assistantChatParams.getSystemMessage()));
            }
            chatMessages.add(UserMessage.from(userContents));
        }
        return chatMessages;
    }
}

package com.moyz.adi.common.interfaces;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.util.AdiDefaultRetrievalAugmentor;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.util.LocalCache;
import com.moyz.adi.common.util.MapDBChatMemoryStore;
import com.moyz.adi.common.vo.*;
import dev.langchain4j.data.message.AiMessage;
import dev.langchain4j.data.message.ChatMessage;
import dev.langchain4j.memory.chat.ChatMemoryProvider;
import dev.langchain4j.memory.chat.MessageWindowChatMemory;
import dev.langchain4j.model.chat.ChatLanguageModel;
import dev.langchain4j.model.chat.StreamingChatLanguageModel;
import dev.langchain4j.model.output.Response;
import dev.langchain4j.rag.DefaultRetrievalAugmentor;
import dev.langchain4j.rag.RetrievalAugmentor;
import dev.langchain4j.rag.content.retriever.ContentRetriever;
import dev.langchain4j.rag.query.transformer.CompressingQueryTransformer;
import dev.langchain4j.rag.query.transformer.QueryTransformer;
import dev.langchain4j.service.AiServices;
import dev.langchain4j.service.TokenStream;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.io.IOException;
import java.net.Proxy;
import java.util.UUID;

import static com.moyz.adi.common.enums.ErrorEnum.B_LLM_SERVICE_DISABLED;

@Slf4j
public abstract class AbstractLLMService<T> {

    protected Proxy proxy;
    protected AiModel aiModel;
    protected T modelPlatformSetting;

    protected AbstractLLMService(AiModel aiModel, String settingName, Class<T> clazz) {
        this.aiModel = aiModel;
        String st = LocalCache.CONFIGS.get(settingName);
        modelPlatformSetting = JsonUtil.fromJson(st, clazz);
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

    protected abstract ChatLanguageModel buildChatLLM(LLMBuilderProperties properties);

    protected abstract StreamingChatLanguageModel buildStreamingChatLLM(LLMBuilderProperties properties);

    protected abstract String parseError(Object error);

    public Response<AiMessage> chat(ChatMessage chatMessage, LLMBuilderProperties properties) {
        if (!isEnabled()) {
            log.error("llm service is disabled");
            throw new BaseException(B_LLM_SERVICE_DISABLED);
        }
        return buildChatLLM(properties).generate(chatMessage);
    }

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
        //TODO 待计算历史对话内容耗费的TOKEN数量，并判断LLM的contextWindow是否容纳[历史对话+用户问题+召回文档]的长度
        if (StringUtils.isNotBlank(assistantChatParams.getMessageId())) {
            ChatMemoryProvider chatMemoryProvider = memoryId -> MessageWindowChatMemory.builder()
                    .id(memoryId)
                    .maxMessages(2)
                    .chatMemoryStore(MapDBChatMemoryStore.getSingleton())
                    .build();
            QueryTransformer queryTransformer = new CompressingQueryTransformer(buildChatLLM(params.getLlmBuilderProperties()));
            RetrievalAugmentor retrievalAugmentor = DefaultRetrievalAugmentor.builder()
                    .queryTransformer(queryTransformer)
                    .contentRetriever(contentRetriever)
                    .build();
            IChatAssistant assistant = AiServices.builder(IChatAssistant.class)
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
     * 普通聊天，将压缩过的用户提问及历史消息发送给AI
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
        //TODO 待计算历史对话内容耗费的TOKEN数量，并判断LLM的contextWindow是否容纳[历史对话+用户问题+召回文档]的长度
        //chat with memory
        if (StringUtils.isNotBlank(assistantChatParams.getMessageId())) {
            ChatMemoryProvider chatMemoryProvider = memoryId -> MessageWindowChatMemory.builder()
                    .id(memoryId)
                    .maxMessages(6)
                    .chatMemoryStore(MapDBChatMemoryStore.getSingleton())
                    .build();
            QueryTransformer queryTransformer = new CompressingQueryTransformer(buildChatLLM(params.getLlmBuilderProperties()));
            RetrievalAugmentor retrievalAugmentor = AdiDefaultRetrievalAugmentor.builder()
                    .queryTransformer(queryTransformer)
                    .build();
            IChatAssistant assistant = AiServices.builder(IChatAssistant.class)
                    .streamingChatLanguageModel(buildStreamingChatLLM(params.getLlmBuilderProperties()))
                    .chatMemoryProvider(chatMemoryProvider)
                    .retrievalAugmentor(retrievalAugmentor)
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
                    if (log.isDebugEnabled()) {
                        log.info("get content:{}", content);
                    }
                    //加空格配合前端的fetchEventSource进行解析，见https://github.com/Azure/fetch-event-source/blob/45ac3cfffd30b05b79fbf95c21e67d4ef59aa56a/src/parse.ts#L129-L133
                    try {
                        params.getSseEmitter().send(" " + content);
                    } catch (IOException e) {
                        log.error("stream onNext error", e);
                    }
                })
                .onComplete((response) -> {
                    log.info("返回数据结束了:{}", response);
                    String questionUuid = StringUtils.isNotBlank(params.getRegenerateQuestionUuid()) ? params.getRegenerateQuestionUuid() : UUID.randomUUID().toString().replace("-", "");
                    PromptMeta questionMeta = new PromptMeta(response.tokenUsage().inputTokenCount(), questionUuid);
                    AnswerMeta answerMeta = new AnswerMeta(response.tokenUsage().outputTokenCount(), UUID.randomUUID().toString().replace("-", ""));
                    ChatMeta chatMeta = new ChatMeta(questionMeta, answerMeta);
                    String meta = JsonUtil.toJson(chatMeta).replace("\r\n", "");
                    log.info("meta:" + meta);
                    try {
                        params.getSseEmitter().send(SseEmitter.event().name(AdiConstant.SSEEventName.DONE).data(" [META]" + meta));
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

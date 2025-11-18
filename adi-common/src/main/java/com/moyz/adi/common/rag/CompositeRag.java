package com.moyz.adi.common.rag;

import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.helper.LLMContext;
import com.moyz.adi.common.helper.SSEEmitterHelper;
import com.moyz.adi.common.interfaces.IStreamingChatAssistant;
import com.moyz.adi.common.interfaces.ITempStreamingChatAssistant;
import com.moyz.adi.common.interfaces.TriConsumer;
import com.moyz.adi.common.memory.shortterm.MapDBChatMemoryStore;
import com.moyz.adi.common.languagemodel.AbstractLLMService;
import com.moyz.adi.common.util.SpringUtil;
import com.moyz.adi.common.vo.*;
import dev.langchain4j.memory.chat.ChatMemoryProvider;
import dev.langchain4j.memory.chat.MessageWindowChatMemory;
import dev.langchain4j.rag.DefaultRetrievalAugmentor;
import dev.langchain4j.rag.RetrievalAugmentor;
import dev.langchain4j.rag.content.retriever.ContentRetriever;
import dev.langchain4j.rag.query.router.DefaultQueryRouter;
import dev.langchain4j.rag.query.router.QueryRouter;
import dev.langchain4j.rag.query.transformer.CompressingQueryTransformer;
import dev.langchain4j.rag.query.transformer.QueryTransformer;
import dev.langchain4j.service.AiServices;
import dev.langchain4j.service.TokenStream;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;

import java.util.ArrayList;
import java.util.List;

import static com.moyz.adi.common.enums.ErrorEnum.B_BREAK_SEARCH;
import static com.moyz.adi.common.enums.ErrorEnum.B_LLM_SERVICE_DISABLED;

/**
 * 组合向量及图谱数据进行RAG
 */
@Slf4j
public class CompositeRag {

    private final EmbeddingRag embeddingRag;
    private final GraphRag graphRag;

    public CompositeRag(String retrieverName) {
        this.embeddingRag = EmbeddingRagContext.get(retrieverName);
        this.graphRag = GraphRagContext.get(retrieverName);
    }

    /**
     * 创建Retriever列表
     *
     * @param param 参数
     * @return ContentRetriever列表
     */
    public List<RetrieverWrapper> createRetriever(RetrieverCreateParam param) {
        List<RetrieverWrapper> retrievers = new ArrayList<>();
        if (null == embeddingRag && null == graphRag) {
            log.warn("No RAG configured");
            return retrievers;
        }
        if (null != embeddingRag) {
            ContentRetriever embeddingRetriever = embeddingRag.createRetriever(param);
            retrievers.add(RetrieverWrapper.builder().contentFrom(embeddingRag.getName()).retriever(embeddingRetriever).response(new ArrayList<>()).build());
        }
        if (null != graphRag) {
            ContentRetriever graphRetriever = graphRag.createRetriever(param);
            retrievers.add(RetrieverWrapper.builder().contentFrom(graphRag.getName()).retriever(graphRetriever).response(new ArrayList<>()).build());
        }
        return retrievers;
    }

    /**
     * 使用RAG处理提问
     *
     * @param retrievers   ContentRetriever列表
     * @param sseAskParams 请求参数
     * @param consumer     回调
     */
    public void ragChat(List<ContentRetriever> retrievers, SseAskParams sseAskParams, TriConsumer<String, PromptMeta, AnswerMeta> consumer) {
        SSEEmitterHelper sseEmitterHelper = SpringUtil.getBean(SSEEmitterHelper.class);
        User user = sseAskParams.getUser();
        String askingKey = sseEmitterHelper.registerEventStreamListener(sseAskParams);
        try {
            query(retrievers, sseAskParams, (response, promptMeta, answerMeta) -> {
                try {
                    consumer.accept(response, promptMeta, answerMeta);
                } catch (Exception e) {
                    log.error("ragProcess error", e);
                } finally {
                    sseEmitterHelper.deleteCache(askingKey);
                }
            });
        } catch (Exception baseException) {
            if (baseException.getCause() instanceof BaseException && B_BREAK_SEARCH.getCode().equals(((BaseException) baseException.getCause()).getCode())) {
                sseEmitterHelper.sendStartAndComplete(user.getId(), sseAskParams.getSseEmitter(), "");
                consumer.accept("", PromptMeta.builder().tokens(0).build(), AnswerMeta.builder().tokens(0).build());
            } else {
                log.error("ragProcess error", baseException);
            }
        }

    }

    /**
     * RAG请求，对prompt进行各种增强后发给AI
     * ps: 挂载了知识库的请求才进行RAG增强
     * <p>
     * TODO...计算并截断超长的请求参数内容（历史记录+向量知识+图谱知识+用户问题+工具）
     *
     * @param retrievers 文档召回器（向量、图谱）
     * @param params     前端传过来的请求参数
     * @param consumer   LLM响应内容的消费者
     */
    private void query(List<ContentRetriever> retrievers, SseAskParams params, TriConsumer<String, PromptMeta, AnswerMeta> consumer) {
        AbstractLLMService llmService = LLMContext.getServiceOrDefault(params.getModelPlatform(), params.getModelName());
        if (!llmService.isEnabled()) {
            log.error("llm service is disabled");
            throw new BaseException(B_LLM_SERVICE_DISABLED);
        }

        QueryRouter queryRouter = new DefaultQueryRouter(retrievers);
        TokenStream tokenStream;
        ChatModelRequestProperties chatModelRequestProperties = params.getChatModelRequestProperties();
        if (StringUtils.isNotBlank(chatModelRequestProperties.getMemoryId())) {
            ChatMemoryProvider chatMemoryProvider = memoryId -> MessageWindowChatMemory.builder()
                    .id(memoryId)
                    .maxMessages(2)
                    .chatMemoryStore(MapDBChatMemoryStore.getSingleton())
                    .build();
            QueryTransformer queryTransformer = new CompressingQueryTransformer(llmService.buildChatLLM(params.getChatModelBuilderProperties()));
            RetrievalAugmentor retrievalAugmentor = DefaultRetrievalAugmentor.builder()
                    .queryTransformer(queryTransformer)
                    .queryRouter(queryRouter)
                    .build();
            IStreamingChatAssistant assistant = AiServices.builder(IStreamingChatAssistant.class)
                    .streamingChatModel(llmService.buildStreamingChatModel(params.getChatModelBuilderProperties()))
                    .retrievalAugmentor(retrievalAugmentor)
                    .chatMemoryProvider(chatMemoryProvider)
                    .build();
            if (StringUtils.isNotBlank(chatModelRequestProperties.getSystemMessage())) {
                tokenStream = assistant.chatWithSystem(chatModelRequestProperties.getMemoryId(), chatModelRequestProperties.getSystemMessage(), chatModelRequestProperties.getUserMessage(), new ArrayList<>());
            } else {
                tokenStream = assistant.chat(chatModelRequestProperties.getMemoryId(), chatModelRequestProperties.getUserMessage(), new ArrayList<>());
            }
        } else {
            ITempStreamingChatAssistant assistant = AiServices.builder(ITempStreamingChatAssistant.class)
                    .streamingChatModel(llmService.buildStreamingChatModel(params.getChatModelBuilderProperties()))
                    .retrievalAugmentor(DefaultRetrievalAugmentor.builder().queryRouter(queryRouter).build())
                    .build();
            if (StringUtils.isNotBlank(chatModelRequestProperties.getSystemMessage())) {
                tokenStream = assistant.chatWithSystem(chatModelRequestProperties.getSystemMessage(), chatModelRequestProperties.getUserMessage(), new ArrayList<>());
            } else {
                tokenStream = assistant.chatSimple(chatModelRequestProperties.getUserMessage(), new ArrayList<>());
            }
        }
        tokenStream
                .onPartialResponse(content -> SSEEmitterHelper.parseAndSendPartialMsg(params.getSseEmitter(), content))
                .onCompleteResponse(response -> {
                    Pair<PromptMeta, AnswerMeta> pair = SSEEmitterHelper.calculateToken(response, params.getUuid());
                    consumer.accept(response.aiMessage().text(), pair.getLeft(), pair.getRight());
                })
                .onError(error -> SSEEmitterHelper.errorAndShutdown(error, params.getSseEmitter()))
                .start();
    }

}

package com.moyz.adi.common.rag;

import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.helper.LLMContext;
import com.moyz.adi.common.helper.SSEEmitterHelper;
import com.moyz.adi.common.service.languagemodel.AbstractLLMService;
import com.moyz.adi.common.interfaces.IStreamingChatAssistant;
import com.moyz.adi.common.interfaces.ITempStreamingChatAssistant;
import com.moyz.adi.common.interfaces.TriConsumer;
import com.moyz.adi.common.util.MapDBChatMemoryStore;
import com.moyz.adi.common.vo.*;
import dev.langchain4j.memory.chat.ChatMemoryProvider;
import dev.langchain4j.memory.chat.MessageWindowChatMemory;
import dev.langchain4j.model.chat.ChatModel;
import dev.langchain4j.rag.DefaultRetrievalAugmentor;
import dev.langchain4j.rag.RetrievalAugmentor;
import dev.langchain4j.rag.content.retriever.ContentRetriever;
import dev.langchain4j.rag.query.router.DefaultQueryRouter;
import dev.langchain4j.rag.query.router.QueryRouter;
import dev.langchain4j.rag.query.transformer.CompressingQueryTransformer;
import dev.langchain4j.rag.query.transformer.QueryTransformer;
import dev.langchain4j.service.AiServices;
import dev.langchain4j.service.TokenStream;
import dev.langchain4j.store.embedding.filter.Filter;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static com.moyz.adi.common.enums.ErrorEnum.B_BREAK_SEARCH;
import static com.moyz.adi.common.enums.ErrorEnum.B_LLM_SERVICE_DISABLED;

/**
 * 组合向量及图谱数据进行RAG
 */
@Component
@Slf4j
public class CompositeRAG {

    @Resource
    private EmbeddingRAG embeddingRAGService;
    @Resource
    private GraphRAG graphRAGService;
    @Resource
    private SSEEmitterHelper sseEmitterHelper;
    @Resource
    private StringRedisTemplate stringRedisTemplate;

    /**
     * 创建Retriever列表
     *
     * @param filter              过滤条件
     * @param maxResults          最大返回数量
     * @param minScore            最小命中分数
     * @param breakIfSearchMissed 如果数据库中搜索不到数据，是否强行中断该搜索，不继续往下执行（即不继续请求LLM进行回答）
     * @return ContentRetriever列表
     */
    public List<ContentRetriever> createRetriever(ChatModel ChatModel, Filter filter, int maxResults, double minScore, boolean breakIfSearchMissed) {
        ContentRetriever contentRetriever1 = embeddingRAGService.createRetriever(filter, maxResults, minScore, breakIfSearchMissed);
        ContentRetriever contentRetriever2 = graphRAGService.createRetriever(ChatModel, filter, maxResults, breakIfSearchMissed);
        return List.of(contentRetriever1, contentRetriever2);
    }

    /**
     * 使用RAG处理提问
     *
     * @param retrievers   ContentRetriver列表
     * @param sseAskParams 请求参数
     * @param consumer     回调
     */
    public void ragChat(List<ContentRetriever> retrievers, SseAskParams sseAskParams, TriConsumer<String, PromptMeta, AnswerMeta> consumer) {
        User user = sseAskParams.getUser();
        String askingKey = sseEmitterHelper.registerEventStreamListener(sseAskParams);
        try {
            query(retrievers, sseAskParams, (response, promptMeta, answerMeta) -> {
                try {
                    consumer.accept(response, promptMeta, answerMeta);
                } catch (Exception e) {
                    log.error("ragProcess error", e);
                } finally {
                    stringRedisTemplate.delete(askingKey);
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
        AbstractLLMService<?> llmService = LLMContext.getLLMServiceByName(params.getModelName());
        if (!llmService.isEnabled()) {
            log.error("llm service is disabled");
            throw new BaseException(B_LLM_SERVICE_DISABLED);
        }

        QueryRouter queryRouter = new DefaultQueryRouter(retrievers);
        TokenStream tokenStream;
        ChatModelParams chatModelParams = params.getChatModelParams();
        if (StringUtils.isNotBlank(chatModelParams.getMemoryId())) {
            ChatMemoryProvider chatMemoryProvider = memoryId -> MessageWindowChatMemory.builder()
                    .id(memoryId)
                    .maxMessages(2)
                    .chatMemoryStore(MapDBChatMemoryStore.getSingleton())
                    .build();
            QueryTransformer queryTransformer = new CompressingQueryTransformer(llmService.buildChatLLM(params.getLlmBuilderProperties()));
            RetrievalAugmentor retrievalAugmentor = DefaultRetrievalAugmentor.builder()
                    .queryTransformer(queryTransformer)
                    .queryRouter(queryRouter)
                    .build();
            IStreamingChatAssistant assistant = AiServices.builder(IStreamingChatAssistant.class)
                    .streamingChatModel(llmService.buildStreamingChatModel(params.getLlmBuilderProperties()))
                    .retrievalAugmentor(retrievalAugmentor)
                    .chatMemoryProvider(chatMemoryProvider)
                    .build();
            if (StringUtils.isNotBlank(chatModelParams.getSystemMessage())) {
                tokenStream = assistant.chatWithSystem(chatModelParams.getMemoryId(), chatModelParams.getSystemMessage(), chatModelParams.getUserMessage(), new ArrayList<>());
            } else {
                tokenStream = assistant.chat(chatModelParams.getMemoryId(), chatModelParams.getUserMessage(), new ArrayList<>());
            }
        } else {
            ITempStreamingChatAssistant assistant = AiServices.builder(ITempStreamingChatAssistant.class)
                    .streamingChatModel(llmService.buildStreamingChatModel(params.getLlmBuilderProperties()))
                    .retrievalAugmentor(DefaultRetrievalAugmentor.builder().queryRouter(queryRouter).build())
                    .build();
            if (StringUtils.isNotBlank(chatModelParams.getSystemMessage())) {
                tokenStream = assistant.chatWithSystem(chatModelParams.getSystemMessage(), chatModelParams.getUserMessage(), new ArrayList<>());
            } else {
                tokenStream = assistant.chatSimple(chatModelParams.getUserMessage(), new ArrayList<>());
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

    public EmbeddingRAG getEmbeddingRAGService() {
        return embeddingRAGService;
    }

    public GraphRAG getGraphRAGService() {
        return graphRAGService;
    }
}

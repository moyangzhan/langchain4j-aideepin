package com.moyz.adi.common.rag;

import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.helper.LLMContext;
import com.moyz.adi.common.helper.SSEEmitterHelper;
import com.moyz.adi.common.interfaces.AbstractLLMService;
import com.moyz.adi.common.interfaces.IStreamingChatAssistant;
import com.moyz.adi.common.interfaces.ITempStreamingChatAssistant;
import com.moyz.adi.common.interfaces.TriConsumer;
import com.moyz.adi.common.util.MapDBChatMemoryStore;
import com.moyz.adi.common.vo.*;
import dev.langchain4j.memory.chat.ChatMemoryProvider;
import dev.langchain4j.memory.chat.MessageWindowChatMemory;
import dev.langchain4j.model.chat.ChatLanguageModel;
import dev.langchain4j.rag.RetrievalAugmentor;
import dev.langchain4j.rag.content.retriever.ContentRetriever;
import dev.langchain4j.rag.query.router.DefaultQueryRouter;
import dev.langchain4j.rag.query.router.QueryRouter;
import dev.langchain4j.rag.query.transformer.CompressingQueryTransformer;
import dev.langchain4j.rag.query.transformer.QueryTransformer;
import dev.langchain4j.service.AiServices;
import dev.langchain4j.service.TokenStream;
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

    public List<ContentRetriever> createRetriever(ChatLanguageModel chatLanguageModel, Map<String, String> metadataCond, int maxResults, double minScore, boolean breakIfSearchMissed) {
        ContentRetriever contentRetriever1 = embeddingRAGService.createRetriever(metadataCond, maxResults, minScore, breakIfSearchMissed);
        ContentRetriever contentRetriever2 = graphRAGService.createRetriever(chatLanguageModel, metadataCond, maxResults, breakIfSearchMissed);
        return List.of(contentRetriever1, contentRetriever2);
    }

    /**
     * 使用RAG处理提问
     *
     * @param retrievers
     * @param sseAskParams
     * @param consumer
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
                sseEmitterHelper.sendAndComplete(user.getId(), sseAskParams.getSseEmitter(), "");
                consumer.accept("", PromptMeta.builder().tokens(0).build(), AnswerMeta.builder().tokens(0).build());
            } else {
                log.error("ragProcess error", baseException);
            }
        }

    }

    /**
     * RAG请求，对prompt进行各种增强后发给AI
     * ps: 挂载了知识库的请求才进行RAG增强
     *
     * @param retrievers
     * @param params
     * @param consumer
     */
    private void query(List<ContentRetriever> retrievers, SseAskParams params, TriConsumer<String, PromptMeta, AnswerMeta> consumer) {
        AbstractLLMService llmService = LLMContext.getLLMServiceByName(params.getModelName());
        if (!llmService.isEnabled()) {
            log.error("llm service is disabled");
            throw new BaseException(B_LLM_SERVICE_DISABLED);
        }

        QueryRouter queryRouter = new DefaultQueryRouter(retrievers);
        AiModel aiModel = llmService.getAiModel();
        TokenStream tokenStream;
        AssistantChatParams assistantChatParams = params.getAssistantChatParams();
        if (StringUtils.isNotBlank(assistantChatParams.getMessageId())) {
            ChatMemoryProvider chatMemoryProvider = memoryId -> MessageWindowChatMemory.builder()
                    .id(memoryId)
                    .maxMessages(2)
                    .chatMemoryStore(MapDBChatMemoryStore.getSingleton())
                    .build();
            QueryTransformer queryTransformer = new CompressingQueryTransformer(llmService.buildChatLLM(params.getLlmBuilderProperties(), params.getUuid()));
            RetrievalAugmentor retrievalAugmentor = AdiKnowledgeBaseRetrievalAugmentor.builder()
                    .queryTransformer(queryTransformer)
                    .queryRouter(queryRouter)
                    .maxInputTokens(aiModel.getMaxInputTokens())
                    .inputAdaptorMsgConsumer(inputAdaptorMsg -> {
                        log.info(inputAdaptorMsg.toString());
                        if (inputAdaptorMsg.getTokenTooMuch() == InputAdaptorMsg.TOKEN_TOO_MUCH_QUESTION) {
                            //TODO 提示用户问题过长
                        }
                    })
                    .build();
            IStreamingChatAssistant assistant = AdiAiServices.builder(IStreamingChatAssistant.class, aiModel.getMaxInputTokens())
                    .streamingChatLanguageModel(llmService.buildStreamingChatLLM(params.getLlmBuilderProperties()))
                    .retrievalAugmentor(retrievalAugmentor)
                    .chatMemoryProvider(chatMemoryProvider)
                    .build();
            if (StringUtils.isNotBlank(assistantChatParams.getSystemMessage())) {
                tokenStream = assistant.chatWithSystem(assistantChatParams.getMessageId(), assistantChatParams.getSystemMessage(), assistantChatParams.getUserMessage(), new ArrayList<>());
            } else {
                tokenStream = assistant.chat(assistantChatParams.getMessageId(), assistantChatParams.getUserMessage(), new ArrayList<>());
            }
        } else {
            ITempStreamingChatAssistant assistant = AiServices.builder(ITempStreamingChatAssistant.class)
                    .streamingChatLanguageModel(llmService.buildStreamingChatLLM(params.getLlmBuilderProperties()))
                    .retrievalAugmentor(AdiKnowledgeBaseRetrievalAugmentor.builder().queryRouter(queryRouter).build())
                    .build();
            if (StringUtils.isNotBlank(assistantChatParams.getSystemMessage())) {
                tokenStream = assistant.chatWithSystem(assistantChatParams.getSystemMessage(), assistantChatParams.getUserMessage(), new ArrayList<>());
            } else {
                tokenStream = assistant.chatSimple(assistantChatParams.getUserMessage(), new ArrayList<>());
            }
        }
        tokenStream
                .onPartialResponse(content -> SSEEmitterHelper.parseAndSendPartialMsg(params.getSseEmitter(), content))
                .onCompleteResponse(response -> {
                    Pair<PromptMeta, AnswerMeta> pair = SSEEmitterHelper.calculateTokenAndShutdown(response, params.getSseEmitter(), params.getUuid(), true);
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

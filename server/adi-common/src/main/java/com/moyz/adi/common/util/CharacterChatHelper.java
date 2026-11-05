package com.moyz.adi.common.util;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.dto.KbInfoResp;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.entity.Character;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.enums.MemoryType;
import com.moyz.adi.common.languagemodel.AbstractLLMService;
import com.moyz.adi.common.rag.AdiEmbeddingStoreContentRetriever;
import com.moyz.adi.common.rag.CompositeRag;
import com.moyz.adi.common.rag.EmbeddingRag;
import com.moyz.adi.common.rag.EmbeddingRagContext;
import com.moyz.adi.common.service.UserMcpService;
import com.moyz.adi.common.util.SpringUtil;
import com.moyz.adi.common.util.AdiStringUtil;
import com.moyz.adi.common.vo.ChatModelBuilderProperties;
import com.moyz.adi.common.vo.ChatModelRequest;
import com.moyz.adi.common.vo.RetrieverCreateParam;
import com.moyz.adi.common.vo.RetrieverWrapper;
import dev.langchain4j.mcp.client.McpClient;
import dev.langchain4j.model.chat.ChatModel;
import dev.langchain4j.rag.content.Content;
import dev.langchain4j.rag.content.retriever.ContentRetriever;
import dev.langchain4j.rag.query.Query;
import dev.langchain4j.store.embedding.filter.Filter;
import dev.langchain4j.store.embedding.filter.comparison.IsEqualTo;
import dev.langchain4j.store.embedding.filter.comparison.IsIn;
import dev.langchain4j.store.embedding.filter.comparison.IsNotEqualTo;
import dev.langchain4j.store.embedding.filter.logical.And;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.core.task.AsyncTaskExecutor;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import static com.moyz.adi.common.cosntant.AdiConstant.MetadataKey.CHARACTER_ID;
import static com.moyz.adi.common.cosntant.AdiConstant.MetadataKey.KB_UUID;
import static com.moyz.adi.common.cosntant.AdiConstant.MetadataKey.MEMORY_TYPE;
import static com.moyz.adi.common.cosntant.AdiConstant.RAG_RETRIEVE_MIN_SCORE_DEFAULT;

/**
 * Character 聊天检索与请求构建辅助，供 Agent 节点与聊天链路共用
 * <p>
 * Character chat retrieval and request building, shared by the agent node and chat pipeline.
 * </p>
 */
@Slf4j
public class CharacterChatHelper {

    private CharacterChatHelper() {
    }

    /**
     * 并发执行语义记忆、情景记忆、知识库三通道检索。
     * semantic 通道排除显式 episodic 记录，缺失 memory_type 视为 semantic（IsNotEqualTo 键缺失时返回 true）。
     * episodic 记忆使用物理隔离的独立向量库。
     * <p>
     * Concurrent retrieval across semantic memory, episodic memory and knowledge bases.
     * The semantic channel excludes explicitly episodic records; rows missing memory_type
     * count as semantic (IsNotEqualTo is true on missing keys).
     * Episodic memory uses its own physically isolated vector store.
     * </p>
     *
     * @param characterId  角色ID
     * @param filteredKb   已过滤的可用知识库
     * @param llmService   大模型服务
     * @param queryText    查询文本
     * @param mainExecutor 异步执行器
     * @return 检索结果列表
     */
    public static List<RetrieverWrapper> retrieve(Long characterId, List<KbInfoResp> filteredKb,
                                                  AbstractLLMService llmService, String queryText,
                                                  AsyncTaskExecutor mainExecutor) {
        ChatModel chatModel = llmService.buildChatLLM(
                ChatModelBuilderProperties.builder()
                        .temperature(AdiConstant.LLM_TEMPERATURE_DEFAULT)
                        .build());

        Filter semanticFilter = new And(
                new IsEqualTo(CHARACTER_ID, characterId),
                new IsNotEqualTo(MEMORY_TYPE, MemoryType.EPISODIC.getDesc())
        );
        RetrieverCreateParam memoryRetrieveParam = RetrieverCreateParam.builder()
                .chatModel(chatModel)
                .filter(semanticFilter)
                .maxResults(3)
                .minScore(RAG_RETRIEVE_MIN_SCORE_DEFAULT)
                .breakIfSearchMissed(false)
                .build();
        List<RetrieverWrapper> retrieverWrappers = new CompositeRag(AdiConstant.RetrieveContentFrom.CHARACTER_MEMORY).createRetriever(memoryRetrieveParam);

        EmbeddingRag episodicRag = EmbeddingRagContext.get(AdiConstant.RetrieveContentFrom.CHARACTER_MEMORY_EPISODIC);
        if (episodicRag != null) {
            RetrieverCreateParam episodicRetrieveParam = RetrieverCreateParam.builder()
                    .filter(new IsEqualTo(CHARACTER_ID, characterId))
                    .maxResults(3)
                    .minScore(RAG_RETRIEVE_MIN_SCORE_DEFAULT)
                    .breakIfSearchMissed(false)
                    .build();
            ContentRetriever episodicRetriever = episodicRag.createRetriever(episodicRetrieveParam);
            retrieverWrappers.add(RetrieverWrapper.builder()
                    .contentFrom(AdiConstant.RetrieveContentFrom.CHARACTER_MEMORY_EPISODIC)
                    .retriever(episodicRetriever)
                    .response(new ArrayList<>())
                    .build());
        }

        if (!filteredKb.isEmpty()) {
            List<String> kbUuids = filteredKb.stream().map(KbInfoResp::getUuid).toList();
            log.info("Preparing to search related knowledge bases, kbUuids:{}, question:{}", String.join(",", kbUuids), queryText);
            RetrieverCreateParam kbRetrieveParam = RetrieverCreateParam.builder()
                    .chatModel(chatModel)
                    .filter(new IsIn(KB_UUID, kbUuids))
                    .maxResults(3)
                    .minScore(RAG_RETRIEVE_MIN_SCORE_DEFAULT)
                    .breakIfSearchMissed(false)
                    .build();
            List<RetrieverWrapper> kbRetrievers = new CompositeRag(AdiConstant.RetrieveContentFrom.KNOWLEDGE_BASE).createRetriever(kbRetrieveParam);
            retrieverWrappers.addAll(kbRetrievers);
        }

        if (!retrieverWrappers.isEmpty()) {
            CountDownLatch countDownLatch = new CountDownLatch(retrieverWrappers.size());
            for (RetrieverWrapper retriever : retrieverWrappers) {
                mainExecutor.execute(() -> {
                    try {
                        List<Content> contents = retriever.getRetriever().retrieve(Query.from(queryText));
                        retriever.setResponse(contents);
                    } catch (Exception e) {
                        log.error("Retrieve content error", e);
                    } finally {
                        countDownLatch.countDown();
                    }
                });
            }
            try {
                boolean awaitRet = countDownLatch.await(1, TimeUnit.MINUTES);
                if (!awaitRet) {
                    log.warn("retrieveContents CountDownLatch await timeout");
                }
            } catch (InterruptedException e) {
                log.error("retrieveContents CountDownLatch await error", e);
                Thread.currentThread().interrupt();
            }
        }
        return retrieverWrappers;
    }

    /**
     * 将检索结果分离为记忆文本与知识文本。
     * 记忆文本融合语义知识与 episodic「过往事件」两个通道。
     * <p>
     * Separate retrieved content into memory text and knowledge text;
     * memory merges the semantic channel and the episodic "past events" channel.
     * </p>
     */
    public static Pair<String, String> buildMemoryAndKnowledge(List<RetrieverWrapper> wrappers) {
        StringBuilder semanticMemory = new StringBuilder();
        StringBuilder episodicMemory = new StringBuilder();
        StringBuilder knowledge = new StringBuilder();
        wrappers.forEach(item -> {
            String retrieveType = item.getContentFrom();
            List<Content> response = item.getResponse();
            if (response == null) {
                return;
            }
            if (AdiConstant.RetrieveContentFrom.CHARACTER_MEMORY.equals(retrieveType)) {
                for (Content content : response) {
                    semanticMemory.append("- ").append(content.textSegment().text()).append("\n");
                }
            } else if (AdiConstant.RetrieveContentFrom.CHARACTER_MEMORY_EPISODIC.equals(retrieveType)) {
                for (Content content : response) {
                    episodicMemory.append("- ").append(content.textSegment().text()).append("\n");
                }
            } else if (AdiConstant.RetrieveContentFrom.KNOWLEDGE_BASE.equals(retrieveType)) {
                for (Content content : response) {
                    knowledge.append(content.textSegment().text()).append("\n");
                }
            }
        });

        StringBuilder memory = new StringBuilder();
        if (semanticMemory.isEmpty() && episodicMemory.isEmpty()) {
            memory.append("None\n");
        } else {
            if (!semanticMemory.isEmpty()) {
                memory.append("[Stable knowledge about the user]\n").append(semanticMemory).append("\n");
            }
            if (!episodicMemory.isEmpty()) {
                memory.append("[Past events you recall]\n").append(episodicMemory).append("\n");
            }
        }
        if (knowledge.isEmpty()) {
            knowledge.append("None\n");
        } else {
            knowledge.append("\n");
        }
        return Pair.of(memory.toString(), knowledge.toString());
    }

    /**
     * 根据 Character 配置构建聊天请求参数
     * <p>
     * Build ChatModelRequest from Character configuration.
     * </p>
     *
     * @param character       角色配置
     * @param userPrompt      处理后的用户提示词
     * @param user            用户
     * @param llmService      LLM 服务
     * @param enableMcp       是否启用 MCP 工具
     * @param enableWebSearch 是否启用联网搜索
     * @param imageUrls       多模态图片 URL 列表
     * @return 聊天请求参数
     */
    public static ChatModelRequest buildChatRequestParams(Character character, String userPrompt,
                                                                User user, AbstractLLMService llmService,
                                                                boolean enableMcp, boolean enableWebSearch,
                                                                List<String> imageUrls) {
        ChatModelRequest.ChatModelRequestBuilder builder = ChatModelRequest.builder();

        if (StringUtils.isNotBlank(character.getAiSystemMessage())) {
            builder.systemMessage(character.getAiSystemMessage());
        }

        if (Boolean.TRUE.equals(character.getUnderstandContextEnable())) {
            builder.memoryId(character.getUuid());
        }

        builder.userMessage(userPrompt);

        if (imageUrls != null && !imageUrls.isEmpty()) {
            builder.imageUrls(imageUrls);
        }

        if (enableMcp && StringUtils.isNotBlank(character.getMcpIds())) {
            UserMcpService userMcpService = SpringUtil.getBean(UserMcpService.class);
            List<Long> mcpIds = AdiStringUtil.stringToList(character.getMcpIds(), ",", Long::parseLong);
            List<McpClient> mcpClients = userMcpService.createMcpClients(character.getUserId(), mcpIds);
            builder.mcpClients(mcpClients);
        }

        AiModel aiModel = llmService.getAiModel();
        Boolean returnThinking = checkIfReturnThinking(aiModel, character);
        builder.returnThinking(returnThinking);

        boolean effectiveWebSearch = enableWebSearch;
        if (effectiveWebSearch && !Boolean.TRUE.equals(aiModel.getIsSupportWebSearch())) {
            log.warn("Web search requested but model {}/{} does not support it; ignoring flag",
                    aiModel.getPlatform(), aiModel.getName());
            effectiveWebSearch = false;
        }
        builder.enableWebSearch(effectiveWebSearch);

        return builder.build();
    }

    /**
     * 判断是否需要返回推理过程
     * <p>
     * Check if thinking/reasoning should be returned.
     * </p>
     *
     * @param aiModel   模型
     * @param character 角色配置
     * @return 是否返回推理过程
     */
    public static Boolean checkIfReturnThinking(AiModel aiModel, Character character) {
        if (!aiModel.getIsReasoner()) {
            return null;
        }
        return Boolean.FALSE.equals(aiModel.getIsThinkingClosable()) || Boolean.TRUE.equals(character.getIsEnableThinking());
    }
}

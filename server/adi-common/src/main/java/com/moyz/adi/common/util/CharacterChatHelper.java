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
 * Character 聊天辅助类
 * <p>
 * 从 {@link com.moyz.adi.common.service.CharacterMessageService} 提取的核心逻辑，
 * 供 Agent 节点和 Character 聊天服务共用。
 * </p>
 * <p>
 * Character chat helper — core logic extracted from CharacterMessageService,
 * shared between Agent node and Character chat service.
 * </p>
 */
@Slf4j
public class CharacterChatHelper {

    private CharacterChatHelper() {
    }

    /**
     * 多知识库搜索、记忆搜索
     * <p>
     * Concurrent RAG retrieval from knowledge bases and character memory.
     * </p>
     *
     * @param characterId 角色ID / Character ID
     * @param filteredKb  有效的已关联的知识库 / Filtered enabled knowledge bases
     * @param llmService  大模型服务 / LLM service
     * @param queryText   查询文本 / Query text
     * @param mainExecutor 异步执行器 / Async task executor
     * @return 检索结果列表 / Retrieval result list
     */
    public static List<RetrieverWrapper> retrieve(Long characterId, List<KbInfoResp> filteredKb,
                                                  AbstractLLMService llmService, String queryText,
                                                  AsyncTaskExecutor mainExecutor) {
        ChatModel chatModel = llmService.buildChatLLM(
                ChatModelBuilderProperties.builder()
                        .temperature(AdiConstant.LLM_TEMPERATURE_DEFAULT)
                        .build());

        //Create memory retriever for SEMANTIC channel.
        //Excludes records explicitly tagged as episodic. Old records without the
        //memory_type metadata (created before this feature) are treated as semantic.
        //<p>
        //创建 SEMANTIC 通道的记忆 retriever。
        //排除显式标记为 episodic 的记录；缺失 memory_type 字段的历史记录视为 semantic（pgvector 的
        //IsNotEqualTo 在键缺失时返回 true，符合预期）。
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

        //Create episodic memory retriever (its own physically isolated vector store).
        //Episodic memories are retrieved alongside semantic ones and presented as a
        //timeline-structured "past events" section in the prompt.
        //<p>
        //创建情景记忆 retriever（独立向量库，与语义记忆物理隔离）。
        //episodic 记忆与 semantic 并行检索，在 prompt 中以时间轴结构的"过去事件"段落呈现。
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

        //Create knowledge base retriever
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

        //Retrieve contents concurrently
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
     * 将检索结果分离为记忆文本和知识文本
     * <p>
     * Separate retrieved content into memory text and knowledge text. Memory text
     * combines two channels: stable semantic knowledge and a timeline-flavored
     * "past events" section for episodic memories. Episodic items are formatted
     * with their content from the vector segment as-is (timestamps/importance/event_type
     * already live in the segment metadata and could be surfaced here later if needed).
     * <p>
     * 将检索结果分离为记忆文本和知识文本。记忆文本融合两个通道：稳定的语义知识 +
     * "过往事件"段（episodic）。
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
     * @param character       角色 / Character
     * @param userPrompt      处理后的用户提示词 / Processed user prompt
     * @param user            用户 / User
     * @param llmService      LLM 服务 / LLM service
     * @param enableMcp       是否启用 MCP 工具 / Enable MCP tools
     * @param enableWebSearch 是否启用联网搜索 / Enable web search
     * @return 聊天请求参数 / Chat request params
     */
    public static ChatModelRequest buildChatRequestParams(Character character, String userPrompt,
                                                                User user, AbstractLLMService llmService,
                                                                boolean enableMcp, boolean enableWebSearch,
                                                                List<String> imageUrls) {
        ChatModelRequest.ChatModelRequestBuilder builder = ChatModelRequest.builder();

        //System message
        if (StringUtils.isNotBlank(character.getAiSystemMessage())) {
            builder.systemMessage(character.getAiSystemMessage());
        }

        //Memory (for context understanding)
        if (Boolean.TRUE.equals(character.getUnderstandContextEnable())) {
            builder.memoryId(character.getUuid());
        }

        //User message
        builder.userMessage(userPrompt);

        //Image URLs (multimodal)
        if (imageUrls != null && !imageUrls.isEmpty()) {
            builder.imageUrls(imageUrls);
        }

        //MCP tools
        if (enableMcp && StringUtils.isNotBlank(character.getMcpIds())) {
            UserMcpService userMcpService = SpringUtil.getBean(UserMcpService.class);
            List<Long> mcpIds = AdiStringUtil.stringToList(character.getMcpIds(), ",", Long::parseLong);
            List<McpClient> mcpClients = userMcpService.createMcpClients(character.getUserId(), mcpIds);
            builder.mcpClients(mcpClients);
        }

        //Thinking
        AiModel aiModel = llmService.getAiModel();
        Boolean returnThinking = checkIfReturnThinking(aiModel, character);
        builder.returnThinking(returnThinking);

        //Web search — drop the flag if the resolved model does not support it,
        //so a stale config or capability change cannot cause a confusing LLM error.
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
     * @param aiModel   模型 / AI model
     * @param character 角色 / Character
     * @return 是否返回推理过程 / Whether to return thinking
     */
    public static Boolean checkIfReturnThinking(AiModel aiModel, Character character) {
        if (!aiModel.getIsReasoner()) {
            return null;
        }
        return Boolean.FALSE.equals(aiModel.getIsThinkingClosable()) || Boolean.TRUE.equals(character.getIsEnableThinking());
    }
}

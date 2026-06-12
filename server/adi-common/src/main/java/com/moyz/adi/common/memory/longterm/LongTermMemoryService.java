package com.moyz.adi.common.memory.longterm;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.LLMCallRecord;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.enums.LLMCallRecordSourceType;
import com.moyz.adi.common.helper.LLMContext;
import com.moyz.adi.common.memory.vo.ActionMemories;
import com.moyz.adi.common.memory.vo.ExtractedFact;
import com.moyz.adi.common.memory.vo.MemoryAddParam;
import com.moyz.adi.common.rag.EmbeddingRagContext;
import com.moyz.adi.common.languagemodel.AbstractLLMService;
import com.moyz.adi.common.service.LLMCallRecordService;
import com.moyz.adi.common.service.UserDayCostService;
import com.moyz.adi.common.util.AdiStringUtil;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.util.UuidUtil;
import com.moyz.adi.common.vo.ChatModelRequest;
import com.moyz.adi.common.vo.EmbeddingIngestParam;
import com.moyz.adi.common.vo.SseAskParam;
import dev.langchain4j.data.document.DefaultDocument;
import dev.langchain4j.data.document.Document;
import dev.langchain4j.data.document.Metadata;
import dev.langchain4j.data.embedding.Embedding;
import dev.langchain4j.data.segment.TextSegment;
import dev.langchain4j.model.chat.response.ChatResponse;
import dev.langchain4j.model.embedding.EmbeddingModel;
import dev.langchain4j.store.embedding.EmbeddingMatch;
import dev.langchain4j.store.embedding.EmbeddingSearchRequest;
import dev.langchain4j.store.embedding.EmbeddingSearchResult;
import dev.langchain4j.store.embedding.EmbeddingStore;
import dev.langchain4j.store.embedding.filter.comparison.IsEqualTo;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.moyz.adi.common.cosntant.AdiConstant.MetadataKey.CHARACTER_ID;
import static com.moyz.adi.common.cosntant.AdiConstant.MetadataKey.CHARACTER_MSG_ID;
import static com.moyz.adi.common.cosntant.AdiConstant.RESPONSE_FORMAT_TYPE_JSON_OBJECT;

/**
 * 长期记忆
 * <p>
 * Long-term memory service for character conversations.
 * Tracks token usage for each internal LLM call and updates user day cost.
 */
@Slf4j
@Service
public class LongTermMemoryService {

    @Resource
    private EmbeddingStore<TextSegment> characterMemoryEmbeddingStore;
    @Resource
    private EmbeddingModel embeddingModel;
    @Resource
    private UserDayCostService userDayCostService;
    @Resource
    private LLMCallRecordService llmCallRecordService;

    @Async
    public void asyncAdd(MemoryAddParam request) {
        log.info("Converting messages to memory, characterId:{}", request.getCharacterId());
        String inputMessage = toInputMessage(request.getUserMessage(), request.getAssistantMessage());
        log.info("inputMessage: {}", inputMessage);
        AbstractLLMService llmService = LLMContext.getServiceOrDefault(request.getModelPlatform(), request.getModelName());

        int totalInputTokens = 0;
        int totalOutputTokens = 0;

        SseAskParam sseAskParam = new SseAskParam();
        sseAskParam.setUuid(UuidUtil.createShort());
        sseAskParam.setHttpRequestParams(
                ChatModelRequest.builder()
                        .systemMessage(LongTermMemoryPrompt.FACT_RETRIEVAL_PROMPT)
                        .userMessage(inputMessage)
                        .responseFormat(RESPONSE_FORMAT_TYPE_JSON_OBJECT)
                        .build()
        );
        sseAskParam.setModelName(request.getModelName());
        sseAskParam.setUser(request.getUser());
        log.info("request:{}", sseAskParam);
        ChatResponse response = llmService.chat(sseAskParam);

        // Track token usage for fact extraction call
        // <p>
        // 追踪事实提取调用的 token 用量
        int[] factTokens = extractTokenUsage(response);
        totalInputTokens += factTokens[0];
        totalOutputTokens += factTokens[1];
        saveCallRecord(request.getUser(), request.getModelPlatform(), request.getModelName(), factTokens[0], factTokens[1], "fact_extraction");

        log.info("Fact extraction response: {}", response.aiMessage().text());
        String factResponse = AdiStringUtil.removeCodeBlock(response.aiMessage().text());
        if (StringUtils.isBlank(factResponse)) {
            log.warn("Unable to extract factual information from this content");
            appendCostSafely(request.getUser(), totalInputTokens + totalOutputTokens, request.isFreeToken());
            return;
        }
        //虽然指定了返回的数据结构，有些模型可能还是会返回无法解析的内容，所以这里需要做下容错处理，先兼容直接返回数组的情况
        List<String> facts;
        if (factResponse.trim().startsWith("[")) {
            facts = JsonUtil.toList(factResponse, String.class);
        } else {
            ExtractedFact extractedFact = JsonUtil.fromJson(factResponse, ExtractedFact.class);
            if (null == extractedFact || CollectionUtils.isEmpty(extractedFact.getFacts())) {
                log.warn("Content cannot be parsed as ExtractedFact, raw content:{}", factResponse);
                appendCostSafely(request.getUser(), totalInputTokens + totalOutputTokens, request.isFreeToken());
                return;
            }
            facts = new ArrayList<>(extractedFact.getFacts());
        }
        if (null == facts) {
            log.warn("Content cannot be parsed as factual information, raw content:{}", factResponse);
            appendCostSafely(request.getUser(), totalInputTokens + totalOutputTokens, request.isFreeToken());
            return;
        }

        //Retrieve old memory
        for (String fact : facts) {
            if (StringUtils.isBlank(fact)) {
                continue;
            }
            Embedding embedding = embeddingModel.embed(fact).content();
            EmbeddingSearchRequest searchRequest = EmbeddingSearchRequest.builder()
                    .queryEmbedding(embedding)
                    .maxResults(5)
                    .minScore(0.7)
                    .filter(new IsEqualTo(CHARACTER_ID, request.getCharacterId()))
                    .build();
            Map<String, EmbeddingMatch<TextSegment>> oldMemoryEmbeddingToContent = new HashMap<>();
            EmbeddingSearchResult<TextSegment> searchResult = characterMemoryEmbeddingStore.search(searchRequest);
            searchResult.matches().forEach(item -> oldMemoryEmbeddingToContent.put(item.embeddingId(), item));

            // mapping UUIDs with integers for handling UUID hallucinations
            Map<Integer, String> tmpIdToEmbeddingId = new HashMap<>();
            List<Map<String, String>> retrievedOldMemory = new ArrayList<>();
            int i = 0;
            for (Map.Entry<String, EmbeddingMatch<TextSegment>> entry : oldMemoryEmbeddingToContent.entrySet()) {
                retrievedOldMemory.add(Map.of("id", String.valueOf(i), "text", entry.getValue().embedded().text()));
                tmpIdToEmbeddingId.put(i, entry.getKey());
                i++;
            }

            // Analyze there is any update/delete/add events required in the memory
            String analyzePrompt = getUpdateMemoryMessages(retrievedOldMemory, facts);
            ChatResponse analyzeResp = llmService.chat(SseAskParam.builder()
                    .uuid(UuidUtil.createShort())
                    .httpRequestParams(
                            ChatModelRequest.builder()
                                    .userMessage(analyzePrompt)
                                    .responseFormat(RESPONSE_FORMAT_TYPE_JSON_OBJECT)
                                    .build()
                    )
                    .modelName(request.getModelName())
                    .user(request.getUser())
                    .build()
            );

            // Track token usage for memory analysis call
            // <p>
            // 追踪记忆分析调用的 token 用量
            int[] analyzeTokens = extractTokenUsage(analyzeResp);
            totalInputTokens += analyzeTokens[0];
            totalOutputTokens += analyzeTokens[1];
            saveCallRecord(request.getUser(), request.getModelPlatform(), request.getModelName(), analyzeTokens[0], analyzeTokens[1], "memory_analysis");

            String resp = analyzeResp.aiMessage().text();
            log.info("Memory analysis response: {}", resp);
            String analyzedMsg = AdiStringUtil.removeCodeBlock(resp);
            ActionMemories actionMemories = JsonUtil.fromJson(analyzedMsg, ActionMemories.class);
            if (null == actionMemories || null == actionMemories.getMemory() || actionMemories.getMemory().isEmpty()) {
                continue;
            }
            for (ActionMemories.ActionMemory actionMemory : actionMemories.getMemory()) {
                if ("NONE".equalsIgnoreCase(actionMemory.getEvent())) {
                    log.info(" No changes required for memory id: {}", actionMemory.getId());
                } else if (AdiConstant.MemoryEvent.DELETE.equalsIgnoreCase(actionMemory.getEvent())) {
                    String embeddingId = tmpIdToEmbeddingId.get(Integer.parseInt(actionMemory.getId()));
                    characterMemoryEmbeddingStore.remove(embeddingId);
                } else if (AdiConstant.MemoryEvent.UPDATE.equalsIgnoreCase(actionMemory.getEvent())) {

                    // Remove by embedding id and re-add with new content
                    String oldEmbeddingId = tmpIdToEmbeddingId.get(Integer.parseInt(actionMemory.getId()));
                    characterMemoryEmbeddingStore.remove(oldEmbeddingId);

                    EmbeddingMatch<TextSegment> match = oldMemoryEmbeddingToContent.get(oldEmbeddingId);
                    Metadata metadata = new Metadata(Map.of(CHARACTER_ID, request.getCharacterId()));
                    TextSegment newSegment = TextSegment.from(actionMemory.getText(), metadata);
                    characterMemoryEmbeddingStore.addAll(List.of(oldEmbeddingId), List.of(match.embedding()), List.of(newSegment));

                } else if (AdiConstant.MemoryEvent.ADD.equalsIgnoreCase(actionMemory.getEvent())) {
                    Metadata metadata = new Metadata(Map.of(CHARACTER_ID, request.getCharacterId()));
                    Document document = new DefaultDocument(actionMemory.getText(), metadata);
                    EmbeddingRagContext.get(AdiConstant.RetrieveContentFrom.CHARACTER_MEMORY).ingest(document,
                            EmbeddingIngestParam.builder()
                                    .overlap(20)
                                    .strategy(AdiConstant.SplitStrategy.RECURSIVE)
                                    .maxSegmentSize(AdiConstant.RAG_MAX_SEGMENT_SIZE_IN_TOKENS)
                                    .customSeparator("")
                                    .build());
                }
            }
        }

        // Update user day cost with total tokens from all LLM calls
        // <p>
        // 将所有 LLM 调用的 token 总量更新到用户日消费
        appendCostSafely(request.getUser(), totalInputTokens + totalOutputTokens, request.isFreeToken());
    }

    /**
     * Extract token usage from ChatResponse.
     * <p>
     * 从 ChatResponse 中提取 token 用量，返回 [inputTokens, outputTokens]。
     */
    private int[] extractTokenUsage(ChatResponse response) {
        if (response != null && response.metadata() != null && response.metadata().tokenUsage() != null) {
            return new int[]{
                    response.metadata().tokenUsage().inputTokenCount() != null
                            ? response.metadata().tokenUsage().inputTokenCount() : 0,
                    response.metadata().tokenUsage().outputTokenCount() != null
                            ? response.metadata().tokenUsage().outputTokenCount() : 0
            };
        }
        return new int[]{0, 0};
    }

    /**
     * Save an LLM call record for long-term memory operations.
     * <p>
     * 保存长期记忆操作的 LLM 调用记录。
     */
    private void saveCallRecord(User user, String modelPlatform, String modelName,
                                int inputTokens, int outputTokens, String stage) {
        if (inputTokens == 0 && outputTokens == 0) {
            return;
        }
        try {
            LLMCallRecord record = new LLMCallRecord();
            record.setUuid(UuidUtil.createShort());
            record.setSourceType(LLMCallRecordSourceType.LONG_TERM_MEMORY.getValue());
            record.setSourceId(0L);
            record.setUserId(user.getId());
            record.setModelPlatform(modelPlatform);
            record.setModelName(modelName);
            record.setInputTokens(inputTokens);
            record.setOutputTokens(outputTokens);
            llmCallRecordService.saveAsync(record);
        } catch (Exception e) {
            log.error("Failed to save LLM call record for long-term memory stage: {}", stage, e);
        }
    }

    /**
     * Append token cost to user day cost, wrapped in try-catch to avoid affecting main flow.
     * <p>
     * 将 token 费用累加到用户日消费，包裹 try-catch 以避免影响主流程。
     */
    private void appendCostSafely(User user, int totalTokens, boolean isFreeToken) {
        if (totalTokens <= 0) {
            return;
        }
        try {
            userDayCostService.appendCostToUser(user, totalTokens, isFreeToken);
        } catch (Exception e) {
            log.error("Failed to append long-term memory cost for userId: {}", user.getId(), e);
        }
    }

    private String toInputMessage(String userMessage, String assistantMessage) {
        return """
                Input:
                user: %s
                assistant: %s
                """.formatted(userMessage, assistantMessage);
    }

    private String getUpdateMemoryMessages(List<Map<String, String>> retrievedOldMemory, List<String> newFacts) {
        String currentMemoryPart;
        if (retrievedOldMemory.isEmpty()) {
            currentMemoryPart = """
                    Current memory is empty.

                    """;
        } else {
            currentMemoryPart = """
                    Below is the current content of my memory which I have collected till now. You have to update it in the following format only:

                    ```
                    %s
                    ```

                    """.formatted(JsonUtil.toJson(retrievedOldMemory));
        }

        return """
                %s

                %s

                The new retrieved facts are mentioned in the triple backticks. You have to analyze the new retrieved facts and determine whether these facts should be added, updated, or deleted in the memory.

                ```
                %s
                ```

                You must return your response in the following JSON structure only:

                {
                    "memory" : [
                        {
                            "id" : "<ID of the memory>",                # Use existing ID for updates/deletes, or new ID for additions
                            "text" : "<Content of the memory>",         # Content of the memory
                            "event" : "<Operation to be performed>",    # Must be "ADD", "UPDATE", "DELETE", or "NONE"
                            "old_memory" : "<Old memory content>"       # Required only if the event is "UPDATE"
                        },
                        ...
                    ]
                }

                Follow the instruction mentioned below:
                - Do not return anything from the custom few shot prompts provided above.
                - If the current memory is empty, then you have to add the new retrieved facts to the memory.
                - You should return the updated memory in only JSON format as shown below. The memory key should be the same if no changes are made.
                - If there is an addition, generate a new key and add the new memory corresponding to it.
                - If there is a deletion, the memory key-value pair should be removed from the memory.
                - If there is an update, the ID key should remain the same and only the value needs to be updated.

                Do not return anything except the JSON format.
                """.formatted(LongTermMemoryPrompt.UPDATE_MEMORY_PROMPT, currentMemoryPart, JsonUtil.toJson(newFacts));
    }


}

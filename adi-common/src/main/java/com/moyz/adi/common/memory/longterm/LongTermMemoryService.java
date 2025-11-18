package com.moyz.adi.common.memory.longterm;

import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.helper.LLMContext;
import com.moyz.adi.common.memory.vo.ActionMemories;
import com.moyz.adi.common.memory.vo.ExtractedFact;
import com.moyz.adi.common.rag.EmbeddingRagContext;
import com.moyz.adi.common.languagemodel.AbstractLLMService;
import com.moyz.adi.common.util.AdiStringUtil;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.util.UuidUtil;
import com.moyz.adi.common.vo.ChatModelRequestProperties;
import com.moyz.adi.common.vo.SseAskParams;
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

import static com.moyz.adi.common.cosntant.AdiConstant.MetadataKey.CONVERSATION_ID;
import static com.moyz.adi.common.cosntant.AdiConstant.RESPONSE_FORMAT_TYPE_JSON;

/**
 * 长期记忆<br/>
 * 目前只支持角色的长期记忆
 */
@Slf4j
@Service
public class LongTermMemoryService {

    @Resource
    private EmbeddingStore<TextSegment> convMemoryEmbeddingStore;
    @Resource
    private EmbeddingModel embeddingModel;

    @Async
    public void asyncAdd(Long convId, String modelPlatform, String modelName, String userMessage, String assistantMessage) {
        log.info("将信息转为记忆，convId: {}", convId);
        String inputMessage = toInputMessage(userMessage, assistantMessage);
        log.info("inputMessage: {}", inputMessage);
        AbstractLLMService llmService = LLMContext.getServiceOrDefault(modelPlatform, modelName);
        SseAskParams sseAskParams = new SseAskParams();
        sseAskParams.setUuid(UuidUtil.createShort());
        sseAskParams.setChatModelRequestProperties(
                ChatModelRequestProperties.builder()
                        .systemMessage(LongTermMemoryPrompt.FACT_RETRIEVAL_PROMPT)
                        .userMessage(inputMessage)
                        .responseFormat(RESPONSE_FORMAT_TYPE_JSON)
                        .build()
        );
        sseAskParams.setModelName(modelName);
        sseAskParams.setUser(ThreadContext.getCurrentUser());
        ChatResponse response = llmService.chat(sseAskParams);
        log.info("Fact extraction response: {}", response.aiMessage().text());
        String factResponse = AdiStringUtil.removeCodeBlock(response.aiMessage().text());
        if (StringUtils.isBlank(factResponse)) {
            log.warn("无法针对本次内容整理出事实性信息");
            return;
        }
        //虽然指定了返回的数据结构，有些模型可能还是会返回无法解析的内容，所以这里需要做下容错处理，先兼容直接返回数组的情况
        List<String> facts;
        if (factResponse.trim().startsWith("[")) {
            facts = JsonUtil.toList(factResponse, String.class);
        } else {
            ExtractedFact extractedFact = JsonUtil.fromJson(factResponse, ExtractedFact.class);
            if (null == extractedFact || CollectionUtils.isEmpty(extractedFact.getFacts())) {
                log.warn("内容无法解析为ExtractedFact对象，原始内容：{}", factResponse);
                return;
            }
            facts = new ArrayList<>(extractedFact.getFacts());
        }
        if (null == facts) {
            log.warn("内容无法解析为事实性信息，原始内容：{}", factResponse);
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
                    .filter(new IsEqualTo(CONVERSATION_ID, convId))
                    .build();
            Map<String, EmbeddingMatch<TextSegment>> oldMemoryEmbeddingToContent = new HashMap<>();
            EmbeddingSearchResult<TextSegment> searchResult = convMemoryEmbeddingStore.search(searchRequest);
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
            String resp = llmService.chat(SseAskParams.builder()
                    .uuid(UuidUtil.createShort())
                    .chatModelRequestProperties(
                            ChatModelRequestProperties.builder()
                                    .userMessage(analyzePrompt)
                                    .responseFormat(RESPONSE_FORMAT_TYPE_JSON)
                                    .build()
                    )
                    .modelName(modelName)
                    .user(ThreadContext.getCurrentUser())
                    .build()
            ).aiMessage().text();
            log.info("Memory analysis response: {}", resp);
            String analyzedMsg = AdiStringUtil.removeCodeBlock(resp);
            ActionMemories actionMemories = JsonUtil.fromJson(analyzedMsg, ActionMemories.class);
            if (null == actionMemories || null == actionMemories.getMemory() || actionMemories.getMemory().isEmpty()) {
                return;
            }
            for (ActionMemories.ActionMemory actionMemory : actionMemories.getMemory()) {
                if ("NONE".equalsIgnoreCase(actionMemory.getEvent())) {
                    log.info(" No changes required for memory id: {}", actionMemory.getId());
                } else if (AdiConstant.MemoryEvent.DELETE.equalsIgnoreCase(actionMemory.getEvent())) {
                    String embeddingId = tmpIdToEmbeddingId.get(Integer.parseInt(actionMemory.getId()));
                    convMemoryEmbeddingStore.remove(embeddingId);
                } else if (AdiConstant.MemoryEvent.UPDATE.equalsIgnoreCase(actionMemory.getEvent())) {

                    // Remove by embedding id and re-add with new content
                    String oldEmbeddingId = tmpIdToEmbeddingId.get(Integer.parseInt(actionMemory.getId()));
                    convMemoryEmbeddingStore.remove(oldEmbeddingId);

                    EmbeddingMatch<TextSegment> match = oldMemoryEmbeddingToContent.get(oldEmbeddingId);
                    Metadata metadata = new Metadata(Map.of(CONVERSATION_ID, convId));
                    TextSegment newSegment = TextSegment.from(actionMemory.getText(), metadata);
                    convMemoryEmbeddingStore.addAll(List.of(oldEmbeddingId), List.of(match.embedding()), List.of(newSegment));

                } else if (AdiConstant.MemoryEvent.ADD.equalsIgnoreCase(actionMemory.getEvent())) {
                    Metadata metadata = new Metadata(Map.of(CONVERSATION_ID, convId));
                    Document document = new DefaultDocument(actionMemory.getText(), metadata);
                    EmbeddingRagContext.get(AdiConstant.RetrieveContentFrom.CONV_MEMORY).ingest(document, 20, null, null);
                }
            }
        }
    }

    public void search(String text) {

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

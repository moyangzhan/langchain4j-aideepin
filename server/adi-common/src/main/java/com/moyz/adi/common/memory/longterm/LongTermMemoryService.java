package com.moyz.adi.common.memory.longterm;

import com.moyz.adi.common.config.AdiProperties;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.LLMCallRecord;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.enums.LLMCallRecordSourceType;
import com.moyz.adi.common.helper.LLMContext;
import com.moyz.adi.common.memory.vo.BatchActionMemories;
import com.moyz.adi.common.memory.vo.ExtractedEpisodicEvent;
import com.moyz.adi.common.memory.vo.ExtractedFact;
import com.moyz.adi.common.memory.vo.ExtractedMemories;
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
import dev.langchain4j.store.embedding.filter.comparison.IsNotEqualTo;
import dev.langchain4j.store.embedding.filter.logical.And;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static com.moyz.adi.common.cosntant.AdiConstant.MetadataKey.CHARACTER_ID;
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
    @Resource
    private AdiProperties adiProperties;
    @Resource
    private EpisodicMemoryService episodicMemoryService;

    @Async
    public void asyncAdd(MemoryAddParam request) {
        log.info("Converting messages to memory, characterId:{}", request.getCharacterId());
        String inputMessage = toInputMessage(request.getUserMessage(), request.getAssistantMessage());
        log.info("inputMessage: {}", inputMessage);
        AbstractLLMService llmService = LLMContext.getServiceOrDefault(request.getModelPlatform(), request.getModelName());

        int totalInputTokens = 0;
        int totalOutputTokens = 0;

        // ===== Stage 1: fact extraction (1 LLM call) =====
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
        ExtractedMemories extracted = parseExtractedMemories(factResponse);
        List<String> facts = extracted.getSemanticFacts() != null
                ? new ArrayList<>(extracted.getSemanticFacts())
                : new ArrayList<>();
        List<ExtractedEpisodicEvent> episodes = extracted.getEpisodicEvents() != null
                ? new ArrayList<>(extracted.getEpisodicEvents())
                : new ArrayList<>();

        // Episodic events: append-only, independent of semantic merge path.
        // <p>
        // Episodic 走独立的 append-only 通道，与 semantic 合并逻辑互不干扰。
        if (!episodes.isEmpty()) {
            try {
                episodicMemoryService.batchAdd(request.getCharacterId(),
                        request.getUser().getId(),
                        request.getSourceMsgId(),
                        episodes,
                        request.isFreeToken());
            } catch (Exception e) {
                log.error("Failed to add episodic memories for characterId={}", request.getCharacterId(), e);
            }
        }

        // Empty semantic facts → finalize cost and exit (episodic already handled above).
        // <p>
        // 没有 semantic fact 时直接结算并返回（episodic 已在上面处理）。
        if (CollectionUtils.isEmpty(facts)) {
            if (episodes.isEmpty()) {
                log.info("No memorable info this round, skipped");
            }
            appendCostSafely(request.getUser(), totalInputTokens + totalOutputTokens, request.isFreeToken());
            return;
        }

        // ===== Stage 2: per-fact embedding search (no LLM, only embedding model) =====
        // For each fact, find its top-K old memories — keep one tmpId space PER fact so the LLM
        // never sees ambiguous IDs across facts.
        // <p>
        // 对每条 fact 做向量检索，每条 fact 维护独立的 tmpId 空间，避免 LLM 跨 fact 时 id 歧义。
        Map<String, List<Map<String, String>>> factToOldMemoriesForPrompt = new LinkedHashMap<>();
        Map<String, Map<String, String>> factToTmpIdToEmbeddingId = new HashMap<>();
        Map<String, Map<String, EmbeddingMatch<TextSegment>>> factToEmbeddingIdToMatch = new HashMap<>();
        double minScore = adiProperties.getMemory() != null
                ? adiProperties.getMemory().getMinScore()
                : AdiConstant.LONG_TERM_MEMORY_MIN_SCORE_DEFAULT;

        for (String fact : facts) {
            if (StringUtils.isBlank(fact)) {
                continue;
            }
            Embedding embedding = embeddingModel.embed(fact).content();
            // Semantic dedup: never let episodic records show up as merge candidates,
            // otherwise the LLM may issue UPDATE/DELETE against append-only episodic memory.
            // <p>
            // 语义去重检索：必须排除 episodic 记录，避免 LLM 对 append-only 的情景记忆下发 UPDATE/DELETE。
            EmbeddingSearchRequest searchRequest = EmbeddingSearchRequest.builder()
                    .queryEmbedding(embedding)
                    .maxResults(5)
                    .minScore(minScore)
                    .filter(new And(
                            new IsEqualTo(CHARACTER_ID, request.getCharacterId()),
                            new IsNotEqualTo(AdiConstant.MetadataKey.MEMORY_TYPE, AdiConstant.MemoryType.EPISODIC)
                    ))
                    .build();
            EmbeddingSearchResult<TextSegment> searchResult = characterMemoryEmbeddingStore.search(searchRequest);

            Map<String, EmbeddingMatch<TextSegment>> embeddingIdToMatch = new LinkedHashMap<>();
            searchResult.matches().forEach(item -> embeddingIdToMatch.put(item.embeddingId(), item));

            List<Map<String, String>> oldMemoryForPrompt = new ArrayList<>();
            Map<String, String> tmpIdToEmbeddingId = new HashMap<>();
            int i = 0;
            for (Map.Entry<String, EmbeddingMatch<TextSegment>> entry : embeddingIdToMatch.entrySet()) {
                String tmpId = String.valueOf(i);
                oldMemoryForPrompt.add(Map.of("id", tmpId, "text", entry.getValue().embedded().text()));
                tmpIdToEmbeddingId.put(tmpId, entry.getKey());
                i++;
            }

            factToOldMemoriesForPrompt.put(fact, oldMemoryForPrompt);
            factToTmpIdToEmbeddingId.put(fact, tmpIdToEmbeddingId);
            factToEmbeddingIdToMatch.put(fact, embeddingIdToMatch);
        }

        if (factToOldMemoriesForPrompt.isEmpty()) {
            log.info("No non-blank fact left to analyze");
            appendCostSafely(request.getUser(), totalInputTokens + totalOutputTokens, request.isFreeToken());
            return;
        }

        // ===== Stage 3: batch memory analysis (1 LLM call for ALL facts) =====
        String analyzePrompt = LongTermMemoryPrompt.buildBatchUpdatePrompt(factToOldMemoriesForPrompt);
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

        int[] analyzeTokens = extractTokenUsage(analyzeResp);
        totalInputTokens += analyzeTokens[0];
        totalOutputTokens += analyzeTokens[1];
        saveCallRecord(request.getUser(), request.getModelPlatform(), request.getModelName(), analyzeTokens[0], analyzeTokens[1], "memory_analysis");

        String resp = analyzeResp.aiMessage().text();
        log.info("Batch memory analysis response: {}", resp);
        String analyzedMsg = AdiStringUtil.removeCodeBlock(resp);
        BatchActionMemories batchActions = parseBatchActions(analyzedMsg);
        if (batchActions == null || CollectionUtils.isEmpty(batchActions.getActions())) {
            log.warn("Batch memory analysis returned empty actions, raw:{}", analyzedMsg);
            appendCostSafely(request.getUser(), totalInputTokens + totalOutputTokens, request.isFreeToken());
            return;
        }

        // ===== Stage 4: apply actions (no LLM) =====
        // Each action.fact tells us which fact-scoped tmpId space to look up.
        // <p>
        // 每个 action 通过 fact 字段定位到对应的 tmpId 空间。
        for (BatchActionMemories.BatchActionMemory action : batchActions.getActions()) {
            try {
                applyAction(action, request.getCharacterId(),
                        factToTmpIdToEmbeddingId, factToEmbeddingIdToMatch);
            } catch (Exception ex) {
                // 单条 action 失败不影响其他 action（关键防御点：parseInt / null 等异常曾整体中断循环）
                log.warn("Apply memory action failed, action={}, error={}", action, ex.getMessage());
            }
        }

        // Update user day cost with total tokens from all LLM calls
        // <p>
        // 将所有 LLM 调用的 token 总量更新到用户日消费
        appendCostSafely(request.getUser(), totalInputTokens + totalOutputTokens, request.isFreeToken());
    }

    /**
     * Apply a single memory action to the vector store. Wrapped in try/catch by caller.
     * <p>
     * 应用单条 memory 操作。由调用方包裹 try/catch，单条失败不影响其他 action。
     */
    private void applyAction(BatchActionMemories.BatchActionMemory action,
                             Long characterId,
                             Map<String, Map<String, String>> factToTmpIdToEmbeddingId,
                             Map<String, Map<String, EmbeddingMatch<TextSegment>>> factToEmbeddingIdToMatch) {
        if (action == null || StringUtils.isBlank(action.getEvent())) {
            return;
        }
        String event = action.getEvent().trim().toUpperCase();
        if (AdiConstant.MemoryEvent.NONE.equalsIgnoreCase(event)) {
            log.info("No changes required for memory id: {}", action.getId());
            return;
        }

        // 对于 ADD，旧记忆映射可能为空，直接 ingest 新内容即可。
        if (AdiConstant.MemoryEvent.ADD.equalsIgnoreCase(event)) {
            if (StringUtils.isBlank(action.getText())) {
                log.warn("ADD action missing text, skip");
                return;
            }
            Metadata metadata = new Metadata(Map.of(
                    CHARACTER_ID, characterId,
                    AdiConstant.MetadataKey.MEMORY_TYPE, AdiConstant.MemoryType.SEMANTIC));
            Document document = new DefaultDocument(action.getText(), metadata);
            EmbeddingRagContext.get(AdiConstant.RetrieveContentFrom.CHARACTER_MEMORY).ingest(document,
                    EmbeddingIngestParam.builder()
                            .overlap(20)
                            .strategy(AdiConstant.SplitStrategy.RECURSIVE)
                            .maxSegmentSize(AdiConstant.RAG_MAX_SEGMENT_SIZE_IN_TOKENS)
                            .customSeparator("")
                            .build());
            return;
        }

        // UPDATE / DELETE 都需要按 fact 定位旧记忆，并校验 id。
        Map<String, String> tmpIdToEmbeddingId = factToTmpIdToEmbeddingId.get(action.getFact());
        Map<String, EmbeddingMatch<TextSegment>> embeddingIdToMatch = factToEmbeddingIdToMatch.get(action.getFact());
        if (tmpIdToEmbeddingId == null || embeddingIdToMatch == null) {
            log.warn("LLM hallucinated fact field, no matching context, fact={}, skip", action.getFact());
            return;
        }
        String tmpId = action.getId();
        if (StringUtils.isBlank(tmpId)) {
            log.warn("UPDATE/DELETE action missing id, action={}, skip", action);
            return;
        }
        String embeddingId = tmpIdToEmbeddingId.get(tmpId);
        if (embeddingId == null) {
            log.warn("LLM hallucinated id, tmpId={} not in mapping, fact={}, skip", tmpId, action.getFact());
            return;
        }

        if (AdiConstant.MemoryEvent.DELETE.equalsIgnoreCase(event)) {
            characterMemoryEmbeddingStore.remove(embeddingId);
            return;
        }

        if (AdiConstant.MemoryEvent.UPDATE.equalsIgnoreCase(event)) {
            if (StringUtils.isBlank(action.getText())) {
                log.warn("UPDATE action missing text, action={}, skip", action);
                return;
            }
            EmbeddingMatch<TextSegment> match = embeddingIdToMatch.get(embeddingId);
            if (match == null) {
                log.warn("UPDATE action embeddingId not found in match map, skip");
                return;
            }
            characterMemoryEmbeddingStore.remove(embeddingId);
            Metadata metadata = new Metadata(Map.of(
                    CHARACTER_ID, characterId,
                    AdiConstant.MetadataKey.MEMORY_TYPE, AdiConstant.MemoryType.SEMANTIC));
            TextSegment newSegment = TextSegment.from(action.getText(), metadata);
            characterMemoryEmbeddingStore.addAll(List.of(embeddingId), List.of(match.embedding()), List.of(newSegment));
            return;
        }

        log.warn("Unknown memory event: {}, action={}", event, action);
    }

    /**
     * Parse the dual-task extraction payload. Falls back gracefully through several formats:
     * <ol>
     *   <li>New {@code {semantic_facts, episodic_events}} dual structure.</li>
     *   <li>Legacy {@code {facts: [...]}} form — populates only semanticFacts.</li>
     *   <li>Raw JSON array — populates only semanticFacts.</li>
     * </ol>
     * Any failure returns an empty {@link ExtractedMemories}; caller decides how to handle empties.
     * <p>
     * 解析双任务提取结果。依次尝试新结构、旧 {facts} 结构、纯数组。解析失败返回空对象。
     */
    private ExtractedMemories parseExtractedMemories(String raw) {
        ExtractedMemories empty = new ExtractedMemories();
        empty.setSemanticFacts(new ArrayList<>());
        empty.setEpisodicEvents(new ArrayList<>());
        if (StringUtils.isBlank(raw)) {
            return empty;
        }

        // Path 1: new dual structure.
        try {
            ExtractedMemories newFormat = JsonUtil.fromJson(raw, ExtractedMemories.class);
            if (newFormat != null
                    && (newFormat.getSemanticFacts() != null || newFormat.getEpisodicEvents() != null)) {
                if (newFormat.getSemanticFacts() == null) {
                    newFormat.setSemanticFacts(new ArrayList<>());
                }
                if (newFormat.getEpisodicEvents() == null) {
                    newFormat.setEpisodicEvents(new ArrayList<>());
                }
                return newFormat;
            }
        } catch (Exception e) {
            log.debug("New ExtractedMemories format parse failed, will try legacy. raw:{}", raw);
        }

        // Path 2 / 3: legacy {facts:[]} or raw array.
        try {
            List<String> facts;
            if (raw.trim().startsWith("[")) {
                facts = JsonUtil.toList(raw, String.class);
            } else {
                ExtractedFact extractedFact = JsonUtil.fromJson(raw, ExtractedFact.class);
                facts = extractedFact != null ? extractedFact.getFacts() : null;
            }
            if (facts != null && !facts.isEmpty()) {
                empty.setSemanticFacts(new ArrayList<>(facts));
            } else {
                log.warn("Content cannot be parsed as facts/memories, raw content:{}", raw);
            }
        } catch (Exception e) {
            log.warn("Failed to parse extraction JSON, raw:{}, err:{}", raw, e.getMessage());
        }
        return empty;
    }

    /**
     * Parse batch action result. Returns null on any parse failure; caller logs and skips.
     * <p>
     * 解析批量 action 结果。任何解析失败都返回 null，调用方记录后跳过本轮。
     */
    private BatchActionMemories parseBatchActions(String json) {
        try {
            return JsonUtil.fromJson(json, BatchActionMemories.class);
        } catch (Exception e) {
            log.warn("Failed to parse BatchActionMemories, raw:{}, err:{}", json, e.getMessage());
            return null;
        }
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

}

package com.moyz.adi.common.memory.longterm;

import com.moyz.adi.common.entity.LLMCallRecord;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.enums.LLMCallRecordSourceType;
import com.moyz.adi.common.helper.LLMContext;
import com.moyz.adi.common.memory.vo.BatchActionMemories;
import com.moyz.adi.common.memory.vo.ExtractedEpisodicEvent;
import com.moyz.adi.common.memory.vo.ExtractedFact;
import com.moyz.adi.common.memory.vo.ExtractedMemories;
import com.moyz.adi.common.memory.vo.MemoryAddParam;
import com.moyz.adi.common.languagemodel.AbstractLLMService;
import com.moyz.adi.common.service.CharacterMemoryInstructionService;
import com.moyz.adi.common.service.LLMCallRecordService;
import com.moyz.adi.common.service.UserDayCostService;
import com.moyz.adi.common.util.AdiStringUtil;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.util.NumberUtil;
import com.moyz.adi.common.util.UuidUtil;
import com.moyz.adi.common.vo.ChatModelRequest;
import com.moyz.adi.common.vo.SseAskParam;
import dev.langchain4j.model.chat.response.ChatResponse;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

import static com.moyz.adi.common.cosntant.AdiConstant.RESPONSE_FORMAT_TYPE_JSON_OBJECT;

/**
 * Long-term memory dispatcher. Owns the LLM extraction round-trip and dispatches
 * the result to {@link SemanticMemoryService} (semantic facts),
 * {@link EpisodicMemoryService} (episodic events) and
 * {@link InstructionMemoryService} (instruction rules; over-limit rule sets are
 * handed to {@link InstructionMemoryCompressor}). Per-call recording + per-call
 * user day-cost accounting happen here in lock-step ("同频结算"): every LLM call
 * is followed by a paired {@code adi_llm_call_record} write and
 * {@code userDayCost} update, so accounts stay accurate even if the flow fails
 * mid-way.
 * <p>
 * 长期记忆调度器。本身负责 LLM 抽取，并把结果分发给 {@link SemanticMemoryService}（语义事实）、
 * {@link EpisodicMemoryService}（情景事件）与 {@link InstructionMemoryService}（指令规则；
 * 规则集超限时交 {@link InstructionMemoryCompressor} 压缩）。LLM 调用记录写库与用户日消费结算"同频"——
 * 每次 LLM 调用之后立刻成对发生，流程中途失败也不会漏算成本。
 */
@Slf4j
@Service
public class LongTermMemoryService {

    @Resource
    private UserDayCostService userDayCostService;
    @Resource
    private LLMCallRecordService llmCallRecordService;
    @Resource
    private SemanticMemoryService semanticMemoryService;
    @Resource
    private EpisodicMemoryService episodicMemoryService;
    @Resource
    private InstructionMemoryService instructionMemoryService;
    @Resource
    private CharacterMemoryInstructionService characterMemoryInstructionService;
    @Resource
    private InstructionMemoryCompressor instructionMemoryCompressor;

    @Async
    public void asyncAdd(MemoryAddParam request) {
        log.info("Converting messages to memory, characterId:{}", request.getCharacterId());
        String inputMessage = toInputMessage(request.getUserMessage(), request.getAssistantMessage());
        log.info("inputMessage: {}", inputMessage);
        AbstractLLMService llmService = LLMContext.getServiceOrDefault(request.getModelPlatform(), request.getModelName());

        // ===== Stage 1: fact extraction (1 LLM call) =====
        long extractionStart = System.currentTimeMillis();
        ChatResponse response = llmService.chat(buildExtractionRequest(request, inputMessage));
        recordLlmCall(request, response, LLMCallRecordSourceType.LONG_TERM_MEMORY_EXTRACTION,
                System.currentTimeMillis() - extractionStart);

        log.info("Fact extraction response: {}", response.aiMessage().text());
        String factResponse = AdiStringUtil.removeCodeBlock(response.aiMessage().text());
        if (StringUtils.isBlank(factResponse)) {
            log.warn("Unable to extract factual information from this content");
            return;
        }
        ExtractedMemories extracted = parseExtractedMemories(factResponse);
        List<String> facts = extracted.getSemanticFacts() != null
                ? new ArrayList<>(extracted.getSemanticFacts())
                : new ArrayList<>();
        List<ExtractedEpisodicEvent> episodes = extracted.getEpisodicEvents() != null
                ? new ArrayList<>(extracted.getEpisodicEvents())
                : new ArrayList<>();
        List<String> instructions = extracted.getInstructions() != null
                ? new ArrayList<>(extracted.getInstructions())
                : new ArrayList<>();

        // Dispatch episodic: append-only, independent of the merge paths.
        // 分发情景事件：append-only，与合并管线互不干扰。
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

        if (CollectionUtils.isEmpty(facts)) {
            if (episodes.isEmpty() && instructions.isEmpty()) {
                log.info("No memorable info this round, skipped");
            }
        } else {
            dispatchSemantic(request, facts, llmService);
        }
        if (!instructions.isEmpty()) {
            dispatchInstructions(request, instructions, llmService);
        }
    }

    /**
     * Semantic channel: per-fact merge-candidate search, one batch LLM analysis for all
     * facts, then apply the actions.
     * <p>
     * 语义通道：按 fact 检索合并候选，一次 LLM 批量分析全部 fact，再应用动作。
     */
    private void dispatchSemantic(MemoryAddParam request, List<String> facts, AbstractLLMService llmService) {
        // ===== Stage 2: per-fact merge-candidate search (embedding only, no LLM) =====
        MergeCandidates candidates =
                semanticMemoryService.searchMergeCandidates(facts, request.getCharacterId());
        if (candidates.isEmpty()) {
            log.info("No non-blank fact left to analyze");
            return;
        }

        // ===== Stage 3: batch memory analysis (1 LLM call for ALL facts) =====
        String analyzePrompt = LongTermMemoryPrompt.buildBatchUpdatePrompt(candidates.factToOldMemoriesForPrompt);
        long analysisStart = System.currentTimeMillis();
        ChatResponse analyzeResp = llmService.chat(buildAnalysisRequest(request, analyzePrompt));
        recordLlmCall(request, analyzeResp, LLMCallRecordSourceType.LONG_TERM_MEMORY_ANALYSIS,
                System.currentTimeMillis() - analysisStart);

        String resp = analyzeResp.aiMessage().text();
        log.info("Batch memory analysis response: {}", resp);
        String analyzedMsg = AdiStringUtil.removeCodeBlock(resp);
        BatchActionMemories batchActions = parseBatchActions(analyzedMsg);
        if (batchActions == null || CollectionUtils.isEmpty(batchActions.getActions())) {
            log.warn("Batch memory analysis returned empty actions, raw:{}", analyzedMsg);
            return;
        }

        // ===== Stage 4: apply semantic actions (no LLM) =====
        semanticMemoryService.applyActions(batchActions, request.getCharacterId(), request.getUser().getId(), candidates);
    }

    /**
     * Instruction channel: mirrors the semantic merge pipeline (candidate search → batch
     * LLM analysis → apply) against the dedicated instruction store; the relational rows
     * keep the source message id. After applying, an over-limit rule set triggers the
     * async compression.
     * <p>
     * instruction 通道：与语义管线同款（候选检索 → 批量 LLM 分析 → 应用），落在独立指令
     * 向量库，关系行保留来源消息 id。应用后若规则集超限则触发异步压缩。
     */
    private void dispatchInstructions(MemoryAddParam request, List<String> instructions, AbstractLLMService llmService) {
        MergeCandidates candidates =
                instructionMemoryService.searchMergeCandidates(instructions, request.getCharacterId());
        if (candidates.isEmpty()) {
            log.info("No non-blank instruction left to analyze");
            return;
        }

        String analyzePrompt = LongTermMemoryPrompt.buildBatchUpdatePrompt(candidates.factToOldMemoriesForPrompt);
        long analysisStart = System.currentTimeMillis();
        ChatResponse analyzeResp = llmService.chat(buildAnalysisRequest(request, analyzePrompt));
        recordLlmCall(request, analyzeResp, LLMCallRecordSourceType.LONG_TERM_MEMORY_ANALYSIS,
                System.currentTimeMillis() - analysisStart);

        String analyzedMsg = AdiStringUtil.removeCodeBlock(analyzeResp.aiMessage().text());
        BatchActionMemories batchActions = parseBatchActions(analyzedMsg);
        if (batchActions == null || CollectionUtils.isEmpty(batchActions.getActions())) {
            log.warn("Instruction analysis returned empty actions, raw:{}", analyzedMsg);
            return;
        }

        instructionMemoryService.applyActions(batchActions, request.getCharacterId(),
                request.getUser().getId(), request.getSourceMsgId(), candidates);

        if (characterMemoryInstructionService.countAliveByCharacter(request.getCharacterId())
                > CharacterMemoryInstructionService.INJECTION_LIMIT) {
            instructionMemoryCompressor.compressAsync(request);
        }
    }

    private SseAskParam buildExtractionRequest(MemoryAddParam request, String inputMessage) {
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
        return sseAskParam;
    }

    private SseAskParam buildAnalysisRequest(MemoryAddParam request, String analyzePrompt) {
        return SseAskParam.builder()
                .uuid(UuidUtil.createShort())
                .httpRequestParams(
                        ChatModelRequest.builder()
                                .userMessage(analyzePrompt)
                                .responseFormat(RESPONSE_FORMAT_TYPE_JSON_OBJECT)
                                .build()
                )
                .modelName(request.getModelName())
                .user(request.getUser())
                .build();
    }

    /**
     * Record this LLM call to {@code adi_llm_call_record} and immediately append
     * its tokens to the user's day-cost (同频结算). Pairing the two writes here
     * eliminates the bookkeeping risk of accumulating tokens and "early settling"
     * on every failure path.
     * <p>
     * 写入 LLM 调用记录并立即累加用户日消费——成对发生，避免"中途失败漏算"的复杂处理。
     *
     * @param sourceType identifies which LLM step produced this call; one value per
     *                   request, so it doubles as the call's business provenance.
     */
    private void recordLlmCall(MemoryAddParam request, ChatResponse response,
                                LLMCallRecordSourceType sourceType, long durationMs) {
        int[] tokens = extractTokenUsage(response);
        LLMCallRecord record = new LLMCallRecord();
        record.setUuid(UuidUtil.createShort());
        record.setSourceType(sourceType.getValue());
        record.setSourceId(request.getSourceMsgId() != null ? request.getSourceMsgId() : 0L);
        record.setUserId(request.getUser().getId());
        record.setModelPlatform(request.getModelPlatform());
        record.setModelName(request.getModelName());
        record.setInputTokens(tokens[0]);
        record.setOutputTokens(tokens[1]);
        record.setDuration(NumberUtil.saturatedCastToInt(durationMs));
        saveCallRecord(record);
        appendCostSafely(request.getUser(), tokens[0] + tokens[1], request.isFreeToken());
    }

    /**
     * Parse the triple-task extraction payload. Falls back gracefully through several formats:
     * <ol>
     *   <li>New {@code {semantic_facts, episodic_events, instructions}} structure.</li>
     *   <li>Legacy {@code {facts: [...]}} form — populates only semanticFacts.</li>
     *   <li>Raw JSON array — populates only semanticFacts.</li>
     * </ol>
     * Any failure returns an empty {@link ExtractedMemories}; caller decides how to handle empties.
     * <p>
     * 解析三任务提取结果。依次尝试新结构、旧 {facts} 结构、纯数组。解析失败返回空对象。
     */
    private ExtractedMemories parseExtractedMemories(String raw) {
        ExtractedMemories empty = new ExtractedMemories();
        empty.setSemanticFacts(new ArrayList<>());
        empty.setEpisodicEvents(new ArrayList<>());
        empty.setInstructions(new ArrayList<>());
        if (StringUtils.isBlank(raw)) {
            return empty;
        }

        // Path 1: new triple structure.
        try {
            ExtractedMemories newFormat = JsonUtil.fromJson(raw, ExtractedMemories.class);
            if (newFormat != null
                    && (newFormat.getSemanticFacts() != null || newFormat.getEpisodicEvents() != null
                    || newFormat.getInstructions() != null)) {
                if (newFormat.getSemanticFacts() == null) {
                    newFormat.setSemanticFacts(new ArrayList<>());
                }
                if (newFormat.getEpisodicEvents() == null) {
                    newFormat.setEpisodicEvents(new ArrayList<>());
                }
                if (newFormat.getInstructions() == null) {
                    newFormat.setInstructions(new ArrayList<>());
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

    private void saveCallRecord(LLMCallRecord record) {
        if (record.getInputTokens() == 0 && record.getOutputTokens() == 0) {
            return;
        }
        try {
            llmCallRecordService.saveAsync(record);
        } catch (Exception e) {
            log.error("Failed to save LLM call record, sourceType: {}", record.getSourceType(), e);
        }
    }

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

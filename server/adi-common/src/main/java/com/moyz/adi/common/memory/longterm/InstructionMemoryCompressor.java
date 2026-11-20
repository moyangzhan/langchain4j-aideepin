package com.moyz.adi.common.memory.longterm;

import com.moyz.adi.common.entity.CharacterMemoryInstruction;
import com.moyz.adi.common.entity.LLMCallRecord;
import com.moyz.adi.common.enums.LLMCallRecordSourceType;
import com.moyz.adi.common.helper.LLMContext;
import com.moyz.adi.common.languagemodel.AbstractLLMService;
import com.moyz.adi.common.memory.vo.InstructionCompressionDirectives;
import com.moyz.adi.common.memory.vo.MemoryAddParam;
import com.moyz.adi.common.service.CharacterMemoryInstructionService;
import com.moyz.adi.common.service.LLMCallRecordService;
import com.moyz.adi.common.service.UserDayCostService;
import com.moyz.adi.common.util.AdiStringUtil;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.util.NumberUtil;
import com.moyz.adi.common.vo.ChatModelRequest;
import com.moyz.adi.common.vo.SseAskParam;
import com.moyz.adi.common.util.UuidUtil;
import dev.langchain4j.model.chat.response.ChatResponse;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import static com.moyz.adi.common.cosntant.AdiConstant.RESPONSE_FORMAT_TYPE_JSON_OBJECT;

/**
 * Async compressor for oversized instruction rule sets: one LLM call over the full
 * rule set returns uuid-keyed edit directives, applied via the merge pipeline's
 * storage half. Per-character debounce: a character already being compressed is not
 * re-triggered; the next over-limit write retries.
 * <p>
 * 指令规则集超限时的异步压缩器：一次 LLM 调用对全量规则产出按 uuid 编辑的指令，经
 * merge 管线的落库半边应用。按角色防抖：压缩进行中不重复触发，下次超限写入再试。
 */
@Slf4j
@Component
public class InstructionMemoryCompressor {

    private static final Set<Long> IN_PROGRESS = ConcurrentHashMap.newKeySet();

    @Resource
    private CharacterMemoryInstructionService characterMemoryInstructionService;
    @Resource
    private InstructionMemoryService instructionMemoryService;
    @Resource
    private LLMCallRecordService llmCallRecordService;
    @Resource
    private UserDayCostService userDayCostService;

    @Async
    public void compressAsync(MemoryAddParam request) {
        Long characterId = request.getCharacterId();
        if (!IN_PROGRESS.add(characterId)) {
            log.info("Instruction compression already running for character {}", characterId);
            return;
        }
        try {
            compress(request);
        } finally {
            IN_PROGRESS.remove(characterId);
        }
    }

    private void compress(MemoryAddParam request) {
        Long characterId = request.getCharacterId();
        List<CharacterMemoryInstruction> aliveRules =
                characterMemoryInstructionService.listAllAliveByCharacter(characterId);
        if (aliveRules.size() <= CharacterMemoryInstructionService.INJECTION_LIMIT) {
            return;
        }
        AbstractLLMService llmService =
                LLMContext.getServiceOrDefault(request.getModelPlatform(), request.getModelName());

        String rulesJson = JsonUtil.toJson(aliveRules.stream()
                .map(rule -> Map.of("uuid", rule.getUuid(), "text", rule.getContent()))
                .toList());
        SseAskParam askParam = new SseAskParam();
        askParam.setUuid(UuidUtil.createShort());
        askParam.setHttpRequestParams(ChatModelRequest.builder()
                .userMessage(LongTermMemoryPrompt.buildInstructionCompressionPrompt(rulesJson))
                .responseFormat(RESPONSE_FORMAT_TYPE_JSON_OBJECT)
                .build());
        askParam.setModelName(request.getModelName());
        askParam.setUser(request.getUser());

        long start = System.currentTimeMillis();
        ChatResponse response = llmService.chat(askParam);
        recordLlmCall(request, response, System.currentTimeMillis() - start);

        InstructionCompressionDirectives directives =
                JsonUtil.fromJson(AdiStringUtil.removeCodeBlock(response.aiMessage().text()),
                        InstructionCompressionDirectives.class);
        instructionMemoryService.applyCompressionDirectives(directives, aliveRules);
        log.info("Instruction compression applied for character {}, rules={}, merges={}, deletes={}",
                characterId, aliveRules.size(),
                directives == null || directives.getMerges() == null ? 0 : directives.getMerges().size(),
                directives == null || directives.getDeletes() == null ? 0 : directives.getDeletes().size());
    }

    private void recordLlmCall(MemoryAddParam request, ChatResponse response, long durationMs) {
        try {
            if (response == null || response.metadata() == null || response.metadata().tokenUsage() == null) {
                return;
            }
            Integer inputTokens = response.metadata().tokenUsage().inputTokenCount();
            Integer outputTokens = response.metadata().tokenUsage().outputTokenCount();
            int in = inputTokens != null ? inputTokens : 0;
            int out = outputTokens != null ? outputTokens : 0;
            if (in == 0 && out == 0) {
                return;
            }
            LLMCallRecord record = new LLMCallRecord();
            record.setUuid(UuidUtil.createShort());
            record.setSourceType(LLMCallRecordSourceType.LONG_TERM_MEMORY_ANALYSIS.getValue());
            record.setSourceId(request.getSourceMsgId() != null ? request.getSourceMsgId() : 0L);
            record.setUserId(request.getUser().getId());
            record.setModelPlatform(request.getModelPlatform());
            record.setModelName(request.getModelName());
            record.setInputTokens(in);
            record.setOutputTokens(out);
            record.setDuration(NumberUtil.saturatedCastToInt(durationMs));
            llmCallRecordService.saveAsync(record);
            userDayCostService.appendCostToUser(request.getUser(), in + out, request.isFreeToken());
        } catch (Exception e) {
            log.error("Failed to record instruction compression LLM call", e);
        }
    }
}

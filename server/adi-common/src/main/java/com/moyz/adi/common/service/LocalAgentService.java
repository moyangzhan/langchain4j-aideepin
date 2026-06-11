package com.moyz.adi.common.service;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.dto.KbInfoResp;
import com.moyz.adi.common.entity.Character;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.helper.LLMContext;
import com.moyz.adi.common.languagemodel.AbstractLLMService;
import com.moyz.adi.common.util.CharacterChatHelper;
import com.moyz.adi.common.util.PromptUtil;
import com.moyz.adi.common.vo.*;

import dev.langchain4j.model.chat.response.ChatResponse;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.core.task.AsyncTaskExecutor;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import static com.moyz.adi.common.enums.ErrorEnum.A_CHARACTER_NOT_FOUND;

/**
 * Local in-process implementation of {@link AgentService}.
 *
 * <p>Loads a Character's configuration and executes the full agent pipeline:
 * RAG retrieval → prompt enhancement → LLM call.</p>
 *
 * <p>Responsibilities end at LLM response — does NOT handle:
 * message persistence, quota deduction, memory storage, or audio/TTS.</p>
 */
@Slf4j
@Service
public class LocalAgentService implements AgentService {

    @Resource
    private CharacterService characterService;

    @Resource
    private AsyncTaskExecutor mainExecutor;

    @Override
    public AgentResult invoke(AgentRequest request, User user, String uuid) {
        // 1. Load Character
        Character character = characterService.lambdaQuery()
                .eq(Character::getUuid, request.getCharacterUuid())
                .eq(Character::getIsDeleted, false)
                .oneOpt()
                .orElseThrow(() -> new BaseException(A_CHARACTER_NOT_FOUND));

        // 2. Resolve LLM service (use config override or system default)
        AbstractLLMService llmService = LLMContext.getServiceOrDefault(
                request.getModelPlatform(), request.getModelName());

        // 3. RAG retrieval (if enabled)
        List<RetrieverWrapper> retrieverWrappers = new ArrayList<>();
        int retrievalCount = 0;
        if (request.isEnableRag()) {
            List<KbInfoResp> filteredKb = new ArrayList<>();
            if (StringUtils.isNotBlank(character.getKbIds())) {
                List<Long> kbIds = Arrays.stream(character.getKbIds().split(","))
                        .map(Long::parseLong).toList();
                filteredKb = characterService.filterEnableKb(user, kbIds);
            }
            retrieverWrappers = CharacterChatHelper.retrieve(
                    character.getId(), filteredKb, llmService, request.getInputText(), mainExecutor);
            retrievalCount = retrieverWrappers.stream()
                    .mapToInt(w -> w.getResponse() != null ? w.getResponse().size() : 0).sum();
        }

        // 4. Prompt enhancement
        Pair<String, String> memoryAndKnowledge = CharacterChatHelper.buildMemoryAndKnowledge(retrieverWrappers);
        String effectiveLocale = StringUtils.isNotBlank(user.getLocale())
                ? user.getLocale()
                : Objects.toString(SysConfigService.getByKey(AdiConstant.SysConfigKey.DEFAULT_LOCALE), "zh-CN");
        String processedPrompt = PromptUtil.createPrompt(
                request.getInputText(), memoryAndKnowledge.getLeft(), memoryAndKnowledge.getRight(), "", effectiveLocale);

        // 5. Build request params (system message + memory + MCP tools + thinking + web search)
        ChatModelRequestParams chatRequestParams = CharacterChatHelper.buildChatRequestParams(
                character, processedPrompt, user, llmService, request.isEnableMcp(), request.isEnableWebSearch(), null);

        // 6. Blocking LLM call
        SseAskParams sseAskParams = new SseAskParams();
        sseAskParams.setUser(user);
        sseAskParams.setUuid(uuid);
        sseAskParams.setModelName(llmService.getAiModel().getName());
        sseAskParams.setHttpRequestParams(chatRequestParams);
        sseAskParams.setModelProperties(
                ChatModelBuilderProperties.builder()
                        .temperature(character.getLlmTemperature())
                        .returnThinking(chatRequestParams.getReturnThinking())
                        .build()
        );

        ChatResponse chatResponse = llmService.chat(sseAskParams);

        // 7. Build result
        Integer inputTokens = null;
        Integer outputTokens = null;
        if (chatResponse.metadata() != null && chatResponse.metadata().tokenUsage() != null) {
            inputTokens = chatResponse.metadata().tokenUsage().inputTokenCount();
            outputTokens = chatResponse.metadata().tokenUsage().outputTokenCount();
        }
        return AgentResult.builder()
                .answer(chatResponse.aiMessage().text())
                .thinking(chatResponse.aiMessage().thinking())
                .inputTokens(inputTokens)
                .outputTokens(outputTokens)
                .retrievalCount(retrievalCount)
                .build();
    }
}

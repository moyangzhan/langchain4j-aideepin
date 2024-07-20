package com.moyz.adi.common.helper;

import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.interfaces.AbstractLLMService;
import lombok.extern.slf4j.Slf4j;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static dev.langchain4j.model.openai.OpenAiModelName.GPT_3_5_TURBO;

/**
 * llmService上下文类（策略模式）
 */
@Slf4j
public class LLMContext {
    public static final Map<String, AbstractLLMService> NAME_TO_LLM_SERVICE = new LinkedHashMap<>();
    private AbstractLLMService llmService;

    public LLMContext(String modelName) {
        if (null == NAME_TO_LLM_SERVICE.get(modelName)) {
            log.warn("︿︿︿ Can not find {}, use the default model GPT_3_5_TURBO ︿︿︿", modelName);
            llmService = NAME_TO_LLM_SERVICE.get(GPT_3_5_TURBO);
        } else {
            llmService = NAME_TO_LLM_SERVICE.get(modelName);
        }
    }

    public static void addLLMService(AbstractLLMService llmService) {
        NAME_TO_LLM_SERVICE.put(llmService.getAiModel().getName(), llmService);
    }

    /**
     * 清除{modelPlatform}下的缓存
     *
     * @param modelPlatform
     */
    public static void clearByPlatform(String modelPlatform) {
        List<String> needDeleted = NAME_TO_LLM_SERVICE.values()
                .stream()
                .filter(item -> item.getAiModel().getPlatform().equalsIgnoreCase(modelPlatform))
                .map(item -> item.getAiModel().getName())
                .collect(Collectors.toList());
        for (String key : needDeleted) {
            log.info("delete llm model service,modelName:{}", key);
            NAME_TO_LLM_SERVICE.remove(key);
        }
    }

    public static void remove(String modelName) {
        NAME_TO_LLM_SERVICE.remove(modelName);
    }

    public static AiModel getAiModel(String modelName) {
        return NAME_TO_LLM_SERVICE.get(modelName).getAiModel();
    }

    public AbstractLLMService getLLMService() {
        return llmService;
    }
}

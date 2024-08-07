package com.moyz.adi.common.helper;

import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.interfaces.AbstractLLMService;
import com.moyz.adi.common.vo.CommonAiPlatformSetting;
import lombok.extern.slf4j.Slf4j;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static dev.langchain4j.model.openai.OpenAiModelName.GPT_3_5_TURBO;

/**
 * llmService上下文类（策略模式）
 */
@Slf4j
public class LLMContext {
    protected static final Map<String, AbstractLLMService> NAME_TO_LLM_SERVICE = new LinkedHashMap<>();

    private LLMContext() {
    }

    public static void addLLMService(AbstractLLMService<CommonAiPlatformSetting> llmService) {
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
                .toList();
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

    public static Map<String, AbstractLLMService> getAllServices() {
        return NAME_TO_LLM_SERVICE;
    }

    public static AbstractLLMService getLLMService(String modelName) {
        AbstractLLMService service = NAME_TO_LLM_SERVICE.get(modelName);
        if (null == service) {
            log.warn("︿︿︿ Can not find {}, use the default model GPT_3_5_TURBO ︿︿︿", modelName);
            return NAME_TO_LLM_SERVICE.get(GPT_3_5_TURBO);
        }
        return service;
    }
}

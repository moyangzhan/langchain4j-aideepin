package com.moyz.adi.common.helper;

import com.moyz.adi.common.interfaces.AbstractImageModelService;
import lombok.extern.slf4j.Slf4j;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static dev.langchain4j.model.openai.OpenAiModelName.DALL_E_2;

/**
 * image model service上下文类（策略模式）
 */
@Slf4j
public class ImageModelContext {

    /**
     * AI图片模型
     */
    public static final Map<String, AbstractImageModelService> NAME_TO_LLM_SERVICE = new HashMap<>();

    private ImageModelContext() {
    }

    public static void addImageModelService(AbstractImageModelService modelService) {
        NAME_TO_LLM_SERVICE.put(modelService.getAiModel().getName(), modelService);
    }

    public static void remove(String modelName) {
        NAME_TO_LLM_SERVICE.remove(modelName);
    }

    public static void clearByPlatform(String platform) {
        List<String> needDeleted = NAME_TO_LLM_SERVICE.values()
                .stream()
                .filter(item -> item.getAiModel().getPlatform().equalsIgnoreCase(platform))
                .map(item -> item.getAiModel().getName())
                .collect(Collectors.toList());
        for (String key : needDeleted) {
            log.info("delete image model service,modelName:{}", key);
            NAME_TO_LLM_SERVICE.remove(key);
        }
    }

    public static AbstractImageModelService getModelService(String modelName) {
        AbstractImageModelService service = NAME_TO_LLM_SERVICE.get(modelName);
        if (null == service) {
            log.warn("︿︿︿ Can not find {}, use the default model DALL_E_2 ︿︿︿", modelName);
            return NAME_TO_LLM_SERVICE.get(DALL_E_2);
        }
        return service;
    }
}

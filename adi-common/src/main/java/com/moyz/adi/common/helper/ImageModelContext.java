package com.moyz.adi.common.helper;

import com.moyz.adi.common.service.languagemodel.AbstractImageModelService;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.List;

import static dev.langchain4j.model.openai.OpenAiImageModelName.DALL_E_2;

/**
 * image model service上下文类（策略模式）
 */
@Slf4j
public class ImageModelContext {

    /**
     * AI图片模型
     */
    public static final List<AbstractImageModelService> LLM_SERVICES = new ArrayList<>();

    private ImageModelContext() {
    }

    public static void addImageModelService(AbstractImageModelService modelService) {
        LLM_SERVICES.add(modelService);
    }

    public static void remove(String modelName) {
        List<AbstractImageModelService> needDeleted = LLM_SERVICES.stream()
                .filter(item -> item.getAiModel().getName().equalsIgnoreCase(modelName))
                .toList();
        LLM_SERVICES.removeAll(needDeleted);
    }

    public static void clearByPlatform(String platform) {
        List<AbstractImageModelService> needDeleted = LLM_SERVICES.stream()
                .filter(item -> item.getAiModel().getPlatform().equalsIgnoreCase(platform))
                .toList();
        LLM_SERVICES.removeAll(needDeleted);
    }

    public static AbstractImageModelService getFirstModelService(String platform) {
        return LLM_SERVICES.stream()
                .filter(item -> item.getAiModel().getPlatform().equalsIgnoreCase(platform) && Boolean.TRUE.equals(item.getAiModel().getIsEnable()))
                .findFirst().orElse(null);
    }

    public static AbstractImageModelService getOrDefault(String modelName) {
        return getBy(modelName, true);
    }

    public static AbstractImageModelService getBy(String modelName, boolean useDefault) {
        AbstractImageModelService service = LLM_SERVICES.stream().filter(item -> item.getAiModel().getName().equalsIgnoreCase(modelName)).findFirst().orElse(null);
        if (null == service && useDefault) {
            log.warn("︿︿︿ Can not find {}, use the default model DALL_E_2 ︿︿︿", modelName);
            return getByModelName(DALL_E_2.toString());
        }
        return service;
    }

    private static AbstractImageModelService getByModelName(String modelName) {
        return LLM_SERVICES.stream().filter(item -> item.getAiModel().getName().equalsIgnoreCase(modelName)).findFirst().orElse(null);
    }
}

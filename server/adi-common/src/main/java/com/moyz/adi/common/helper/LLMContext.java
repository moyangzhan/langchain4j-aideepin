package com.moyz.adi.common.helper;

import com.aliyun.core.utils.StringUtils;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.languagemodel.AbstractLLMService;
import com.moyz.adi.common.service.ModelHealthService;
import com.moyz.adi.common.util.SpringUtil;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * llmService上下文类（策略模式）
 * LLM service context class (Strategy pattern)
 */
@Slf4j
public class LLMContext {

    protected static final List<AbstractLLMService> LLM_SERVICES = new ArrayList<>();

    private LLMContext() {
    }

    public static void addLLMService(AbstractLLMService llmService) {
        LLM_SERVICES.add(llmService);
    }

    /**
     * 清除{modelPlatform}下的缓存
     * Clear cache under {modelPlatform}
     *
     * @param modelPlatform 模型所属的平台 / The platform the model belongs to
     */
    public static void clearByPlatform(String modelPlatform, String modelType) {
        List<AbstractLLMService> readyToDelete = LLM_SERVICES.stream()
                .filter(item -> item.getAiModel().getPlatform().equalsIgnoreCase(modelPlatform) && item.getAiModel().getType().equalsIgnoreCase(modelType))
                .toList();
        for (AbstractLLMService key : readyToDelete) {
            log.info("delete llm model service,modelName:{}", key.getAiModel().getName());
            LLM_SERVICES.remove(key);
        }
    }

    public static void remove(String platform, String modelName) {
        List<AbstractLLMService> readyToDelete = LLM_SERVICES.stream()
                .filter(item -> item.getPlatform().getName().equals(platform) && item.getAiModel().getName().equalsIgnoreCase(modelName))
                .toList();
        for (AbstractLLMService key : readyToDelete) {
            log.info("delete llm model service,modelName:{}", key.getAiModel().getName());
            LLM_SERVICES.remove(key);
        }
    }

    /**
     * 以 {platform} 和 {modelName} 定位一个 AiModel
     * Locate an AiModel by {platform} and {modelName}
     * 不能只使用 {modelName} 来获取 AiModel，以防不同平台有相同名字的模型
     * Cannot use only {modelName} to get AiModel, in case different platforms have models with the same name
     *
     * @param platform  模型平台名称 / Model platform name
     * @param modelName 模型名称 / Model name
     * @return AiModel
     */
    public static AiModel getAiModel(String platform, String modelName) {
        return LLM_SERVICES.stream()
                .filter(item -> (StringUtils.isBlank(platform) || item.getPlatform().getName().equals(platform)) && item.getAiModel().getName().equalsIgnoreCase(modelName))
                .findFirst()
                .map(AbstractLLMService::getAiModel)
                .orElse(null);
    }

    public static List<AbstractLLMService> getAllServices() {
        return LLM_SERVICES;
    }

    public static AbstractLLMService getServiceOrDefault(String platform, String modelName) {
        return getServiceByPlatformAndModel(platform, modelName, true);
    }

    public static AbstractLLMService getServiceById(Long modelId, boolean useDefault) {
        ModelHealthService healthService = SpringUtil.getBean(ModelHealthService.class);
        AbstractLLMService service = LLM_SERVICES.stream()
                .filter(item -> item.getAiModel().getId().equals(modelId)
                        && healthService.isHealthy(item.getAiModel().getName()))
                .findFirst()
                .orElse(null);
        if (null == service && useDefault) {
            Optional<AbstractLLMService> serviceOptional = getFirstEnableAndFree();
            if (serviceOptional.isPresent()) {
                log.warn("^^^^^ modelId:{} not found or unhealthy, using the first available free model {} ^^^^^", modelId, serviceOptional.get().getAiModel().getName());
                return serviceOptional.get();
            }
            log.error("^^^^^ No available models, please check platform and model configuration ^^^^^");
            throw new BaseException(ErrorEnum.A_ENABLE_MODEL_NOT_FOUND);
        }
        return service;
    }

    /**
     * 以 {platform} 和 {modelName} 查找 AbstractLLMService，如果找不到，则返回第1个可用的免费模型
     * Find AbstractLLMService by {platform} and {modelName}, return the first available free model if not found
     * PS: 不能只使用 {modelName} 来获取 AbstractLLMService，以防不同平台有相同名字的模型
     * PS: Cannot use only {modelName} to get AbstractLLMService, in case different platforms have models with the same name
     *
     * @param platform   模型平台名称 / Model platform name
     * @param modelName  模型名称 / Model name
     * @param useDefault 如果找不到，是否使用第1个可用的免费模型 / Whether to use the first available free model if not found
     * @return AbstractLLMService
     */
    public static AbstractLLMService getServiceByPlatformAndModel(String platform, String modelName, boolean useDefault) {
        ModelHealthService healthService = SpringUtil.getBean(ModelHealthService.class);
        AbstractLLMService service = LLM_SERVICES.stream()
                .filter(item -> (StringUtils.isBlank(platform) || item.getPlatform().getName().equals(platform))
                        && item.getAiModel().getName().equalsIgnoreCase(modelName)
                        && healthService.isHealthy(item.getAiModel().getName()))
                .findFirst()
                .orElse(null);
        if (null == service && useDefault) {
            Optional<AbstractLLMService> serviceOptional = getFirstEnableAndFree();
            if (serviceOptional.isPresent()) {
                log.warn("^^^^^ {} not found or unhealthy, using the first available free model {} ^^^^^", modelName, serviceOptional.get().getAiModel().getName());
                return serviceOptional.get();
            }
            log.error("^^^^^ No available models, please check platform and model configuration ^^^^^");
            throw new BaseException(ErrorEnum.A_ENABLE_MODEL_NOT_FOUND);
        }
        return service;
    }

    /**
     * 选择顺序：
     * 一、优先选择免费可用的模型；
     * 二、收费可用的模型
     * Selection order:
     * 1. Prefer free and available models;
     * 2. Paid and available models
     *
     * @return 返回免费或收费的可用模型 / Return a free or paid available model
     */
    public static Optional<AbstractLLMService> getFirstEnableAndFree() {
        ModelHealthService healthService = SpringUtil.getBean(ModelHealthService.class);
        AbstractLLMService freeObj = null;
        AbstractLLMService enableObj = null;
        for (AbstractLLMService service : LLM_SERVICES) {
            AiModel aiModel = service.getAiModel();
            if (Boolean.TRUE.equals(aiModel.getIsEnable()) && healthService.isHealthy(aiModel.getName())) {
                if (Boolean.TRUE.equals(aiModel.getIsFree())) {
                    freeObj = service;
                    break;
                } else if (null == enableObj) {
                    enableObj = service;
                }
            }
        }
        if (null != freeObj) {
            return Optional.of(freeObj);
        } else if (null != enableObj) {
            return Optional.of(enableObj);
        }
        return Optional.empty();
    }
}

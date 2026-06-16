package com.moyz.adi.chat.controller;

import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.entity.ModelPlatform;
import com.moyz.adi.common.helper.ImageModelContext;
import com.moyz.adi.common.helper.LLMContext;
import com.moyz.adi.common.service.ModelHealthService;
import com.moyz.adi.common.service.ModelPlatformService;
import com.moyz.adi.common.languagemodel.data.ImageModelInfo;
import com.moyz.adi.common.languagemodel.data.LLMModelInfo;
import com.moyz.adi.common.util.SpringUtil;
import io.swagger.v3.oas.annotations.Operation;
import jakarta.annotation.Resource;
import org.springframework.beans.BeanUtils;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/model")
public class ModelController {

    @Resource
    private ModelPlatformService modelPlatformService;

    @Operation(summary = "支持的大语言模型列表 | Supported LLM List")
    @GetMapping(value = "/llms")
    public List<LLMModelInfo> llms() {
        ModelHealthService healthService = SpringUtil.getBean(ModelHealthService.class);
        Map<String, ModelHealthService.HealthCheckResult> allStatuses = healthService.getAllStatuses();
        return LLMContext.getAllServices().stream().map(item -> {
            AiModel aiModel = item.getAiModel();
            LLMModelInfo modelInfo = new LLMModelInfo();
            modelInfo.setModelId(aiModel.getId());
            modelInfo.setModelName(aiModel.getName());
            modelInfo.setModelTitle(aiModel.getTitle());
            modelInfo.setModelPlatform(aiModel.getPlatform());
            modelInfo.setEnable(aiModel.getIsEnable());
            BeanUtils.copyProperties(aiModel, modelInfo);
            // 健康状态（不过滤，前端根据状态置灰）
            ModelHealthService.HealthCheckResult health = allStatuses.get(aiModel.getName());
            if (health != null) {
                modelInfo.setHealthStatus(health.getStatus().name());
                modelInfo.setHealthReason(health.getFailReason());
            }
            return modelInfo;
        }).toList();
    }

    @Operation(summary = "支持的图片模型列表 | Supported Image Model List")
    @GetMapping(value = "/imageModels")
    public List<ImageModelInfo> imageModels() {
        return ImageModelContext.LLM_SERVICES.stream().map(item -> {
            AiModel aiModel = item.getAiModel();
            ImageModelInfo modelInfo = new ImageModelInfo();
            modelInfo.setModelId(aiModel.getId());
            modelInfo.setModelName(aiModel.getName());
            modelInfo.setModelTitle(aiModel.getTitle());
            modelInfo.setModelPlatform(aiModel.getPlatform());
            modelInfo.setEnable(aiModel.getIsEnable());
            BeanUtils.copyProperties(aiModel, modelInfo);
            return modelInfo;
        }).toList();
    }

    @Operation(summary = "模型平台列表 | Model Platform List")
    @GetMapping(value = "/platforms")
    public List<ModelPlatform> platforms() {
        List<ModelPlatform> platforms = modelPlatformService.listAll();
        platforms.forEach(item -> {
            item.setApiKey("");
            item.setSecretKey("");
        });
        return platforms;
    }
}

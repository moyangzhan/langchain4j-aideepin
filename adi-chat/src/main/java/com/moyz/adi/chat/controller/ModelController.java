package com.moyz.adi.chat.controller;

import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.helper.ImageModelContext;
import com.moyz.adi.common.helper.LLMContext;
import com.moyz.adi.common.vo.ImageModelInfo;
import com.moyz.adi.common.vo.LLMModelInfo;
import io.swagger.v3.oas.annotations.Operation;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;
import java.util.stream.Collectors;

@RestController
@RequestMapping("/model")
public class ModelController {
    @Operation(summary = "支持的大语言模型列表")
    @GetMapping(value = "/llms")
    public List<LLMModelInfo> llms() {
        return LLMContext.getAllServices().values().stream().map(item -> {
            AiModel aiModel = item.getAiModel();
            LLMModelInfo modelInfo = new LLMModelInfo();
            modelInfo.setModelId(aiModel.getId());
            modelInfo.setModelName(aiModel.getName());
            modelInfo.setModelPlatform(aiModel.getPlatform());
            modelInfo.setEnable(item.getAiModel().getIsEnable());
            return modelInfo;
        }).collect(Collectors.toList());
    }

    @Operation(summary = "支持的图片模型列表")
    @GetMapping(value = "/imageModels")
    public List<ImageModelInfo> imageModels() {
        return ImageModelContext.NAME_TO_LLM_SERVICE.values().stream().map(item -> {
            AiModel aiModel = item.getAiModel();
            ImageModelInfo modelInfo = new ImageModelInfo();
            modelInfo.setModelId(aiModel.getId());
            modelInfo.setModelName(aiModel.getName());
            modelInfo.setModelPlatform(aiModel.getPlatform());
            modelInfo.setEnable(item.getAiModel().getIsEnable());
            return modelInfo;
        }).collect(Collectors.toList());
    }
}

package com.moyz.adi.common.service;

import com.moyz.adi.common.config.AdiProperties;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.helper.AsrModelContext;
import com.moyz.adi.common.helper.ImageModelContext;
import com.moyz.adi.common.helper.LLMContext;
import com.moyz.adi.common.helper.TtsModelContext;
import com.moyz.adi.common.searchengine.GoogleSearchEngineService;
import com.moyz.adi.common.searchengine.SearchEngineServiceContext;
import com.moyz.adi.common.service.languagemodel.*;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;

import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import static com.moyz.adi.common.util.LocalCache.MODEL_ID_TO_OBJ;

@Slf4j
@Service
public class AiModelSettingService {

    @Resource
    private AdiProperties adiProperties;

    private InetSocketAddress proxyAddress;

    private List<AiModel> all = new ArrayList<>();

    /**
     * 模型及其配置初始化
     *
     * @param allModels
     */
    public void init(List<AiModel> allModels) {
        this.all = allModels;
        for (AiModel model : all) {
            if (Boolean.TRUE.equals(model.getIsEnable())) {
                MODEL_ID_TO_OBJ.put(model.getId(), model);
            } else {
                MODEL_ID_TO_OBJ.remove(model.getId());
            }
        }
        if (adiProperties.getProxy().isEnable()) {
            proxyAddress = new InetSocketAddress(adiProperties.getProxy().getHost(), adiProperties.getProxy().getHttpPort());
        } else {
            proxyAddress = null;
        }

        initLLMServiceList();
        initImageModelServiceList();
        initAsrModelServiceList();
        initTtsModelServiceList();
    }

    /**
     * 初始化大语言模型列表
     */
    private synchronized void initLLMServiceList() {

        //deepseek
        initLLMService(AdiConstant.ModelPlatform.DEEPSEEK, DeepSeekLLMService::new);

        //openai
        initLLMService(AdiConstant.ModelPlatform.OPENAI, model -> new OpenAiLLMService(model).setProxyAddress(proxyAddress));

        //dashscope
        initLLMService(AdiConstant.ModelPlatform.DASHSCOPE, DashScopeLLMService::new);

        //qianfan
        initLLMService(AdiConstant.ModelPlatform.QIANFAN, QianFanLLMService::new);

        //ollama
        initLLMService(AdiConstant.ModelPlatform.OLLAMA, OllamaLLMService::new);
        // 硅基流动
        initLLMService(AdiConstant.ModelPlatform.SILICONFLOW, SiliconflowLLMService::new);
    }

    /**
     * 初始化文生图模型、搜索服务
     */
    private synchronized void initImageModelServiceList() {
        //openai image model
        initImageModelService(AdiConstant.ModelPlatform.OPENAI, model -> new OpenAiDalleService(model).setProxyAddress(proxyAddress));


        initImageModelService(AdiConstant.ModelPlatform.DASHSCOPE, DashScopeWanxService::new);

        //search engine
        SearchEngineServiceContext.addWebSearcher(AdiConstant.SearchEngineName.GOOGLE, new GoogleSearchEngineService(proxyAddress));
    }

    private synchronized void initAsrModelServiceList() {
        initAsrModelService(AdiConstant.ModelPlatform.DASHSCOPE, DashScopeAsrService::new);
        initAsrModelService(AdiConstant.ModelPlatform.SILICONFLOW, SiliconflowAsrService::new);
    }

    private synchronized void initTtsModelServiceList() {
        initTtsModelService(AdiConstant.ModelPlatform.DASHSCOPE, DashScopeTtsService::new);
    }

    private void initLLMService(String platform, Function<AiModel, AbstractLLMService<?>> function) {
        List<AiModel> models = all.stream().filter(item -> item.getType().equals(AdiConstant.ModelType.TEXT) && item.getPlatform().equals(platform)).toList();
        if (CollectionUtils.isEmpty(models)) {
            log.warn("{} service is disabled", platform);
        }
        LLMContext.clearByPlatform(platform);
        for (AiModel model : models) {
            log.info("add llm model,model:{}", model);
            LLMContext.addLLMService(function.apply(model));
        }
    }

    private void initImageModelService(String platform, Function<AiModel, AbstractImageModelService<?>> function) {
        List<AiModel> models = all.stream().filter(item -> item.getType().equals(AdiConstant.ModelType.IMAGE) && item.getPlatform().equals(platform)).toList();
        if (CollectionUtils.isEmpty(models)) {
            log.warn("{} service is disabled", platform);
        }
        ImageModelContext.clearByPlatform(platform);
        for (AiModel model : models) {
            log.info("add image model,model:{}", model);
            ImageModelContext.addImageModelService(function.apply(model));
        }

    }

    private void initAsrModelService(String platform, Function<AiModel, AbstractAsrModelService<?>> function) {
        List<AiModel> models = all.stream().filter(item -> item.getType().equals(AdiConstant.ModelType.ASR) && item.getPlatform().equals(platform)).toList();
        if (CollectionUtils.isEmpty(models)) {
            log.warn("{} service is disabled", platform);
        }
        AsrModelContext.clearByPlatform(platform);
        for (AiModel model : models) {
            log.info("add asr model,model:{}", model);
            AsrModelContext.addService(function.apply(model));
        }
    }

    private void initTtsModelService(String platform, Function<AiModel, AbstractTtsModelService<?>> function) {
        List<AiModel> models = all.stream().filter(item -> item.getType().equals(AdiConstant.ModelType.TTS) && item.getPlatform().equals(platform)).toList();
        if (CollectionUtils.isEmpty(models)) {
            log.warn("{} service is disabled", platform);
        }
        TtsModelContext.clearByPlatform(platform);
        for (AiModel model : models) {
            log.info("add tts model,model:{}", model);
            TtsModelContext.addService(function.apply(model));
        }
    }


    public void delete(AiModel aiModel) {
        LLMContext.remove(aiModel.getName());
        ImageModelContext.remove(aiModel.getName());
        MODEL_ID_TO_OBJ.remove(aiModel.getId());
    }

    public void addOrUpdate(AiModel aiModel) {
        AiModel existOne = all.stream().filter(item -> item.getId().equals(aiModel.getId())).findFirst().orElse(null);
        if (null == existOne) {
            all.add(aiModel);
        } else {
            BeanUtils.copyProperties(aiModel, existOne);
        }
        if (AdiConstant.ModelType.TEXT.equalsIgnoreCase(aiModel.getType())) {
            initLLMServiceList();
        } else {
            initImageModelServiceList();
        }
        MODEL_ID_TO_OBJ.put(aiModel.getId(), aiModel);
    }
}

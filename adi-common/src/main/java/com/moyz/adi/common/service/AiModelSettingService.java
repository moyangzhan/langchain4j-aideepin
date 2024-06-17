package com.moyz.adi.common.service;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.helper.ImageModelContext;
import com.moyz.adi.common.helper.LLMContext;
import com.moyz.adi.common.interfaces.AbstractImageModelService;
import com.moyz.adi.common.interfaces.AbstractLLMService;
import com.moyz.adi.common.searchengine.GoogleSearchEngine;
import com.moyz.adi.common.searchengine.SearchEngineContext;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.net.InetSocketAddress;
import java.net.Proxy;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.moyz.adi.common.util.LocalCache.MODEL_ID_TO_OBJ;

@Slf4j
@Service
public class AiModelSettingService {

    @Value("${adi.proxy.enable:false}")
    protected boolean proxyEnable;

    @Value("${adi.proxy.host:0}")
    protected String proxyHost;

    @Value("${adi.proxy.http-port:0}")
    protected int proxyHttpPort;

    private Proxy proxy;

    @Resource
    private RAGService queryCompressingRagService;

    private List<AiModel> all = new ArrayList<>();

    public void init(List<AiModel> allModels) {
        this.all = allModels;
        for (AiModel model : all) {
            if (model.getIsEnable()) {
                MODEL_ID_TO_OBJ.put(model.getId(), model);
            } else {
                MODEL_ID_TO_OBJ.remove(model.getId());
            }
        }
        if (proxyEnable) {
            proxy = new Proxy(Proxy.Type.HTTP, new InetSocketAddress(proxyHost, proxyHttpPort));
        } else {
            proxy = null;
        }

        initLLMServiceList();
        initImageModelServiceList();
    }

    /**
     * 初始化大语言模型列表
     */
    private synchronized void initLLMServiceList() {

        //openai
        initLLMService(AdiConstant.ModelPlatform.OPENAI, (model) -> new OpenAiLLMService(model).setProxy(proxy).setQueryCompressingRAGService(queryCompressingRagService));

        //dashscope
        initLLMService(AdiConstant.ModelPlatform.DASHSCOPE, (model) -> new DashScopeLLMService(model).setQueryCompressingRAGService(queryCompressingRagService));

        //qianfan
        initLLMService(AdiConstant.ModelPlatform.QIANFAN, (model) -> new QianFanLLMService(model).setQueryCompressingRAGService(queryCompressingRagService));

        //ollama
        initLLMService(AdiConstant.ModelPlatform.OLLAMA, (model) -> new OllamaLLMService(model).setQueryCompressingRAGService(queryCompressingRagService));
    }

    /**
     * 初始化生图模型列表
     */
    private synchronized void initImageModelServiceList() {
        //openai image model
        initImageModelService(AdiConstant.ModelPlatform.OPENAI, (model) -> new OpenAiImageModelService(model).setProxy(proxy));

        //search engine
        SearchEngineContext.addEngine(AdiConstant.SearchEngineName.GOOGLE, new GoogleSearchEngine().setProxy(proxy));
    }

    private void initLLMService(String platform, Function<AiModel, AbstractLLMService> function) {
        List<AiModel> models = all.stream().filter(item -> item.getType().equals(AdiConstant.ModelType.TEXT) && item.getPlatform().equals(platform)).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(models)) {
            log.warn("{} service is disabled", platform);
        }
        LLMContext.clearByPlatform(platform);
        for (AiModel model : models) {
            log.info("add llm model,model:{}", model);
            LLMContext.addLLMService(function.apply(model));
        }
    }

    private void initImageModelService(String platform, Function<AiModel, AbstractImageModelService> function) {
        List<AiModel> models = all.stream().filter(item -> item.getType().equals(AdiConstant.ModelType.IMAGE) && item.getPlatform().equals(platform)).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(models)) {
            log.warn("{} service is disabled", platform);
        }
        ImageModelContext.clearByPlatform(platform);
        for (AiModel model : models) {
            log.info("add image model,model:{}", model);
            ImageModelContext.addImageModelService(function.apply(model));
        }

    }

    public void delete(AiModel aiModel) {
        LLMContext.remove(aiModel.getName());
        ImageModelContext.remove(aiModel.getName());
        MODEL_ID_TO_OBJ.remove(aiModel.getId());
    }

    public void add(AiModel aiModel) {
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

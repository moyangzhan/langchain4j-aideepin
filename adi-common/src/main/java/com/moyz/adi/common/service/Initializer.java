package com.moyz.adi.common.service;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.helper.ImageModelContext;
import com.moyz.adi.common.helper.LLMContext;
import com.moyz.adi.common.interfaces.AbstractImageModelService;
import com.moyz.adi.common.interfaces.AbstractLLMService;
import com.moyz.adi.common.searchengine.GoogleSearchEngine;
import com.moyz.adi.common.searchengine.SearchEngineContext;
import jakarta.annotation.PostConstruct;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.net.InetSocketAddress;
import java.net.Proxy;
import java.util.List;
import java.util.function.Function;

@Slf4j
@Service
public class Initializer {

    @Value("${adi.proxy.enable:false}")
    protected boolean proxyEnable;

    @Value("${adi.proxy.host:0}")
    protected String proxyHost;

    @Value("${adi.proxy.http-port:0}")
    protected int proxyHttpPort;

    @Resource
    private AiModelService aiModelService;

    @Resource
    private SysConfigService sysConfigService;

    @Resource
    private RAGService queryCompressingRagService;

    @PostConstruct
    public void init() {
        sysConfigService.reload();
        aiModelService.initAll();

        Proxy proxy;
        if (proxyEnable) {
            proxy = new Proxy(Proxy.Type.HTTP, new InetSocketAddress(proxyHost, proxyHttpPort));
        } else {
            proxy = null;
        }

        //openai
        initLLMService(AdiConstant.ModelPlatform.OPENAI, (model) -> new OpenAiLLMService(model).setProxy(proxy).setQueryCompressingRAGService(queryCompressingRagService));

        //dashscope
        initLLMService(AdiConstant.ModelPlatform.DASHSCOPE, (model) -> new DashScopeLLMService(model).setQueryCompressingRAGService(queryCompressingRagService));

        //qianfan
        initLLMService(AdiConstant.ModelPlatform.QIANFAN, (model) -> new QianFanLLMService(model).setQueryCompressingRAGService(queryCompressingRagService));

        //ollama
        initLLMService(AdiConstant.ModelPlatform.OLLAMA, (model) -> new OllamaLLMService(model).setQueryCompressingRAGService(queryCompressingRagService));

        //openai image model
        initImageModelService(AdiConstant.ModelPlatform.OPENAI, (model) -> new OpenAiImageModelService(model).setProxy(proxy));

        //search engine
        SearchEngineContext.addEngine(AdiConstant.SearchEngineName.GOOGLE, new GoogleSearchEngine().setProxy(proxy));

    }

    private void initLLMService(String platform, Function<AiModel, AbstractLLMService> function) {
        List<AiModel> models = aiModelService.listBy(platform, AdiConstant.ModelType.TEXT);
        if (CollectionUtils.isEmpty(models)) {
            log.warn("{} service is disabled", platform);
        }
        for (AiModel model : models) {
            LLMContext.addLLMService(function.apply(model));
        }
    }

    private void initImageModelService(String platform, Function<AiModel, AbstractImageModelService> function) {
        List<AiModel> models = aiModelService.listBy(platform, AdiConstant.ModelType.IMAGE);
        if (CollectionUtils.isEmpty(models)) {
            log.warn("{} service is disabled", platform);
        }
        for (AiModel model : models) {
            ImageModelContext.addImageModelService(function.apply(model));
        }

    }
}

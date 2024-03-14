package com.moyz.adi.common.service;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.helper.ImageModelContext;
import com.moyz.adi.common.helper.LLMContext;
import dev.langchain4j.model.openai.OpenAiModelName;
import jakarta.annotation.PostConstruct;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.net.InetSocketAddress;
import java.net.Proxy;

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
    private SysConfigService sysConfigService;

    @Resource
    private RAGService ragService;

    @PostConstruct
    public void init() {
        sysConfigService.reload();

        Proxy proxy = null;
        if (proxyEnable) {
            proxy = new Proxy(Proxy.Type.HTTP, new InetSocketAddress(proxyHost, proxyHttpPort));
        }

        //openai
        String[] openaiModels = LLMContext.getSupportModels(AdiConstant.SysConfigKey.OPENAI_SETTING);
        if(openaiModels.length == 0){
            log.warn("openai service is disabled");
        }
        for (String model : openaiModels) {
            LLMContext.addLLMService(model, new OpenAiLLMService(model, proxy));
        }

        //dashscope
        String[] dashscopeModels = LLMContext.getSupportModels(AdiConstant.SysConfigKey.DASHSCOPE_SETTING);
        if(dashscopeModels.length == 0){
            log.warn("dashscope service is disabled");
        }
        for (String model : dashscopeModels) {
            LLMContext.addLLMService(model, new DashScopeLLMService(model));
        }

        //qianfan
        String[] qianfanModels = LLMContext.getSupportModels(AdiConstant.SysConfigKey.QIANFAN_SETTING);
        if(qianfanModels.length == 0){
            log.warn("qianfan service is disabled");
        }
        for (String model : qianfanModels) {
            LLMContext.addLLMService(model, new QianFanLLMService(model));
        }

        //ollama
        String[] ollamaModels = LLMContext.getSupportModels(AdiConstant.SysConfigKey.OLLAMA_SETTING);
        if(ollamaModels.length == 0){
            log.warn("ollama service is disabled");
        }
        for (String model : ollamaModels) {
            LLMContext.addLLMService("ollama:" + model, new OllamaLLMService(model));
        }

        ImageModelContext.addImageModelService(OpenAiModelName.DALL_E_2, new OpenAiImageModelService(OpenAiModelName.DALL_E_2, proxy));


        ragService.init();
    }
}

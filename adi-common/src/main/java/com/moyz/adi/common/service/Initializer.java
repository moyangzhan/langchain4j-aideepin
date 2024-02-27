package com.moyz.adi.common.service;

import com.moyz.adi.common.helper.ImageModelContext;
import com.moyz.adi.common.helper.LLMContext;
import dev.langchain4j.model.dashscope.QwenModelName;
import dev.langchain4j.model.openai.OpenAiModelName;
import jakarta.annotation.PostConstruct;
import jakarta.annotation.Resource;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.net.InetSocketAddress;
import java.net.Proxy;

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
        LLMContext.addLLMService(OpenAiModelName.GPT_3_5_TURBO, new OpenAiLLMService(OpenAiModelName.GPT_3_5_TURBO, proxy));
        LLMContext.addLLMService(QwenModelName.QWEN_MAX, new DashScopeLLMService(QwenModelName.QWEN_MAX, proxy));
        ImageModelContext.addImageModelService(OpenAiModelName.DALL_E_2, new OpenAiImageModelService(OpenAiModelName.DALL_E_2, proxy));


        ragService.init();
    }
}

package com.moyz.adi.common.service;

import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.interfaces.AbstractLLMService;
import com.moyz.adi.common.vo.LLMBuilderProperties;
import com.moyz.adi.common.vo.LLMException;
import com.moyz.adi.common.vo.OllamaSetting;
import dev.langchain4j.model.chat.ChatLanguageModel;
import dev.langchain4j.model.chat.StreamingChatLanguageModel;
import dev.langchain4j.model.ollama.OllamaChatModel;
import dev.langchain4j.model.ollama.OllamaStreamingChatModel;
import org.apache.commons.lang3.StringUtils;

import static com.moyz.adi.common.cosntant.AdiConstant.SysConfigKey.OLLAMA_SETTING;

public class OllamaLLMService extends AbstractLLMService<OllamaSetting> {

    public OllamaLLMService(AiModel aiModel) {
        super(aiModel, OLLAMA_SETTING, OllamaSetting.class);
    }

    @Override
    public boolean isEnabled() {
        return StringUtils.isNotBlank(modelPlatformSetting.getBaseUrl()) && aiModel.getIsEnable();
    }

    @Override
    protected ChatLanguageModel doBuildChatLLM(LLMBuilderProperties properties) {
        return OllamaChatModel.builder()
                .baseUrl(modelPlatformSetting.getBaseUrl())
                .modelName(aiModel.getName())
                .temperature(properties.getTemperature())
                .build();
    }

    @Override
    public StreamingChatLanguageModel buildStreamingChatLLM(LLMBuilderProperties properties) {
        double temperature = properties.getTemperatureWithDefault(0.7);
        return OllamaStreamingChatModel.builder()
                .baseUrl(modelPlatformSetting.getBaseUrl())
                .modelName(aiModel.getName())
                .temperature(temperature)
                .build();
    }

    @Override
    protected LLMException parseError(Object error) {
        return null;
    }
}

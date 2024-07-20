package com.moyz.adi.common.service;

import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.interfaces.AbstractLLMService;
import com.moyz.adi.common.vo.LLMBuilderProperties;
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
    protected ChatLanguageModel buildChatLLM(LLMBuilderProperties properties) {
        double temperature = 0.7;
        if (null != properties && properties.getTemperature() > 0 && properties.getTemperature() <= 1) {
            temperature = properties.getTemperature();
        }
        return OllamaChatModel.builder()
                .baseUrl(modelPlatformSetting.getBaseUrl())
                .modelName(aiModel.getName())
                .temperature(temperature)
                .build();
    }

    @Override
    protected StreamingChatLanguageModel buildStreamingChatLLM(LLMBuilderProperties properties) {
        double temperature = 0.7;
        if (null != properties && properties.getTemperature() > 0 && properties.getTemperature() <= 1) {
            temperature = properties.getTemperature();
        }
        return OllamaStreamingChatModel.builder()
                .baseUrl(modelPlatformSetting.getBaseUrl())
                .modelName(aiModel.getName())
                .temperature(temperature)
                .build();
    }

    @Override
    protected String parseError(Object error) {
        return null;
    }
}

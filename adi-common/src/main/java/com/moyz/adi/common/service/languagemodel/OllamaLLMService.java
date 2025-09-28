package com.moyz.adi.common.service.languagemodel;

import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.entity.ModelPlatform;
import com.moyz.adi.common.vo.ChatModelBuilderProperties;
import com.moyz.adi.common.vo.LLMException;
import dev.langchain4j.model.TokenCountEstimator;
import dev.langchain4j.model.chat.ChatModel;
import dev.langchain4j.model.chat.StreamingChatModel;
import dev.langchain4j.model.ollama.OllamaChatModel;
import dev.langchain4j.model.ollama.OllamaStreamingChatModel;
import org.apache.commons.lang3.StringUtils;

public class OllamaLLMService extends AbstractLLMService {

    public OllamaLLMService(AiModel aiModel, ModelPlatform modelPlatform) {
        super(aiModel, modelPlatform);
    }

    @Override
    public boolean isEnabled() {
        return StringUtils.isNotBlank(platform.getBaseUrl()) && aiModel.getIsEnable();
    }

    @Override
    protected ChatModel doBuildChatModel(ChatModelBuilderProperties properties) {
        return OllamaChatModel.builder()
                .baseUrl(platform.getBaseUrl())
                .modelName(aiModel.getName())
                .temperature(properties.getTemperature())
                .build();
    }

    @Override
    public StreamingChatModel buildStreamingChatModel(ChatModelBuilderProperties properties) {
        double temperature = properties.getTemperatureWithDefault(0.7);
        return OllamaStreamingChatModel.builder()
                .baseUrl(platform.getBaseUrl())
                .modelName(aiModel.getName())
                .temperature(temperature)
                .build();
    }

    @Override
    public TokenCountEstimator getTokenEstimator() {
        return null;
    }

    @Override
    protected LLMException parseError(Object error) {
        return null;
    }
}

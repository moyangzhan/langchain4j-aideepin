package com.moyz.adi.common.service.languagemodel;

import com.knuddels.jtokkit.api.ModelType;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.entity.ModelPlatform;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.util.OpenAiUtil;
import com.moyz.adi.common.vo.ChatModelBuilderProperties;
import com.moyz.adi.common.vo.LLMException;
import dev.langchain4j.http.client.jdk.JdkHttpClient;
import dev.langchain4j.model.TokenCountEstimator;
import dev.langchain4j.model.chat.ChatModel;
import dev.langchain4j.model.chat.StreamingChatModel;
import dev.langchain4j.model.chat.request.ChatRequestParameters;
import dev.langchain4j.model.openai.OpenAiChatModel;
import dev.langchain4j.model.openai.OpenAiChatRequestParameters;
import dev.langchain4j.model.openai.OpenAiStreamingChatModel;
import dev.langchain4j.model.openai.OpenAiTokenCountEstimator;
import lombok.experimental.Accessors;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;

import java.net.ProxySelector;
import java.net.http.HttpClient;
import java.time.Duration;
import java.time.temporal.ChronoUnit;
import java.util.Map;

/**
 * OpenAi LLM service
 */
@Slf4j
@Accessors(chain = true)
public class OpenAiLLMService extends AbstractLLMService {

    public OpenAiLLMService(AiModel model, ModelPlatform modelPlatform) {
        super(model, modelPlatform);
    }

    @Override
    public boolean isEnabled() {
        return StringUtils.isNotBlank(platform.getApiKey()) && aiModel.getIsEnable();
    }

    @Override
    protected ChatModel doBuildChatModel(ChatModelBuilderProperties properties) {
        if (StringUtils.isBlank(platform.getApiKey())) {
            throw new BaseException(ErrorEnum.B_LLM_SECRET_KEY_NOT_SET);
        }
        OpenAiChatModel.OpenAiChatModelBuilder builder = OpenAiChatModel.builder()
                .baseUrl(platform.getBaseUrl())
                .modelName(aiModel.getName())
                .temperature(properties.getTemperature())
                .maxRetries(1)
                .timeout(Duration.of(60, ChronoUnit.SECONDS))
                .apiKey(platform.getApiKey());
        if (StringUtils.isNotBlank(platform.getBaseUrl())) {
            builder.baseUrl(platform.getBaseUrl());
        }
        if (null != proxyAddress && platform.getIsProxyEnable()) {
            HttpClient.Builder httpClientBuilder = HttpClient.newBuilder().proxy(ProxySelector.of(proxyAddress));
            builder.httpClientBuilder(JdkHttpClient.builder().httpClientBuilder(httpClientBuilder));
        }
        return builder.build();
    }

    @Override
    public StreamingChatModel buildStreamingChatModel(ChatModelBuilderProperties properties) {
        if (StringUtils.isBlank(platform.getApiKey())) {
            throw new BaseException(ErrorEnum.B_LLM_SECRET_KEY_NOT_SET);
        }
        double temperature = properties.getTemperatureWithDefault(0.7);
        OpenAiStreamingChatModel.OpenAiStreamingChatModelBuilder builder = OpenAiStreamingChatModel
                .builder()
                .baseUrl(platform.getBaseUrl())
                .modelName(aiModel.getName())
                .temperature(temperature)
                .apiKey(platform.getApiKey())
                .returnThinking(properties.getReturnThinking())
                .timeout(Duration.of(60, ChronoUnit.SECONDS));
        if (null != proxyAddress && platform.getIsProxyEnable()) {
            HttpClient.Builder httpClientBuilder = HttpClient.newBuilder().proxy(ProxySelector.of(proxyAddress));
            builder.httpClientBuilder(JdkHttpClient.builder().httpClientBuilder(httpClientBuilder));
        }
        return builder.build();
    }

    @Override
    protected ChatRequestParameters doCreateChatRequestParameters(ChatRequestParameters defaultParameters, Map<String, Object> customParameters) {
        // 兼容 OpenAi api 的平台可能会需要自定义参数
        if (null != customParameters && !customParameters.isEmpty()) {
            ChatRequestParameters openAiChatRequestParameters = OpenAiChatRequestParameters.builder()
                    .customParameters(customParameters)
                    .build();
            return openAiChatRequestParameters.overrideWith(defaultParameters);
        }
        return super.doCreateChatRequestParameters(defaultParameters, customParameters);
    }

    @Override
    public TokenCountEstimator getTokenEstimator() {
        if (aiModel.getPlatform().equals(AdiConstant.ModelPlatform.OPENAI)) {
            return new OpenAiTokenCountEstimator(aiModel.getName());
        }
        return new OpenAiTokenCountEstimator(ModelType.GPT_3_5_TURBO.getName());
    }

    @Override
    protected LLMException parseError(Object error) {
        return OpenAiUtil.parseError(error);
    }

}

package com.moyz.adi.common.languagemodel;

import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.entity.ModelPlatform;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.vo.ChatModelBuilderProperties;
import com.moyz.adi.common.languagemodel.data.LLMException;
import com.moyz.adi.common.vo.QianFanAiModelSetting;
import dev.langchain4j.community.model.qianfan.QianfanChatModel;
import dev.langchain4j.community.model.qianfan.QianfanStreamingChatModel;
import dev.langchain4j.model.TokenCountEstimator;
import dev.langchain4j.model.chat.ChatModel;
import dev.langchain4j.model.chat.StreamingChatModel;
import lombok.experimental.Accessors;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

/**
 * QianFan LLM service
 */
@Slf4j
@Accessors(chain = true)
public class QianFanLLMService extends AbstractLLMService {

    private String endpoint = "";

    public QianFanLLMService(AiModel aiModel, ModelPlatform modelPlatform) {
        super(aiModel, modelPlatform);
        String ms = aiModel.getSetting();
        if (StringUtils.isNotBlank(ms)) {
            QianFanAiModelSetting aiModelSetting = JsonUtil.fromJson(ms, QianFanAiModelSetting.class);
            if (null != aiModelSetting && StringUtils.isNotBlank(aiModelSetting.getEndpoint())) {
                endpoint = aiModelSetting.getEndpoint();
            }
        }
    }

    @Override
    public boolean isEnabled() {
        return StringUtils.isNoneBlank(platform.getApiKey(), platform.getSecretKey()) && aiModel.getIsEnable();
    }

    @Override
    protected ChatModel doBuildChatModel(ChatModelBuilderProperties properties) {
        QianfanChatModel.QianfanChatModelBuilder builder = QianfanChatModel.builder()
                .baseUrl(StringUtils.isBlank(platform.getBaseUrl()) ? null : platform.getBaseUrl())
                .modelName(aiModel.getName())
                .temperature(properties.getTemperature())
                .topP(1.0)
                .maxRetries(1)
                .apiKey(platform.getApiKey())
                .secretKey(platform.getSecretKey())
                .logRequests(true)
                .logResponses(true);
        if (StringUtils.isNotBlank(endpoint)) {
            builder.endpoint(endpoint);
        }
        return builder.build();
    }

    @Override
    public StreamingChatModel buildStreamingChatModel(ChatModelBuilderProperties properties) {
        double temperature = properties.getTemperatureWithDefault(0.7);
        QianfanStreamingChatModel.QianfanStreamingChatModelBuilder builder = QianfanStreamingChatModel.builder()
                .baseUrl(platform.getBaseUrl())
                .modelName(aiModel.getName())
                .temperature(temperature)
                .topP(1.0)
                .apiKey(platform.getApiKey())
                .secretKey(platform.getSecretKey())
                .logRequests(true)
                .logResponses(true);
        if (StringUtils.isNotBlank(endpoint)) {
            builder.endpoint(endpoint);
        }
        return builder.build();
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

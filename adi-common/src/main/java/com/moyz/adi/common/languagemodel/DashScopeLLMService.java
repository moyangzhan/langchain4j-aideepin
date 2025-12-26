package com.moyz.adi.common.languagemodel;

import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.entity.ModelPlatform;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.util.DashscopeUtil;
import com.moyz.adi.common.vo.ChatModelBuilderProperties;
import com.moyz.adi.common.languagemodel.data.LLMException;
import com.moyz.adi.common.vo.SseAskParams;
import dev.langchain4j.community.model.dashscope.QwenChatModel;
import dev.langchain4j.community.model.dashscope.QwenChatRequestParameters;
import dev.langchain4j.community.model.dashscope.QwenStreamingChatModel;
import dev.langchain4j.community.model.dashscope.QwenTokenCountEstimator;
import dev.langchain4j.model.TokenCountEstimator;
import dev.langchain4j.model.chat.ChatModel;
import dev.langchain4j.model.chat.StreamingChatModel;
import dev.langchain4j.model.chat.request.ChatRequestParameters;
import dev.langchain4j.model.openai.OpenAiChatModelName;
import dev.langchain4j.model.openai.OpenAiTokenCountEstimator;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;

import java.util.Map;

import static com.moyz.adi.common.cosntant.AdiConstant.CustomChatRequestParameterKeys.ENABLE_WEB_SEARCH;
import static com.moyz.adi.common.cosntant.AdiConstant.CustomChatRequestParameterKeys.ENABLE_THINKING;
import static com.moyz.adi.common.enums.ErrorEnum.B_LLM_SECRET_KEY_NOT_SET;

/**
 * 灵积模型服务(DashScope LLM service) <br/>
 * Dashscope 的 OpenAI 兼容api格式为：https://dashscope.aliyuncs.com/compatible-mode/v1/chat/completions <br/>
 * Dashscope 的 SDK 的 api 格式为：https://dashscope.aliyuncs.com/api/v1/services/aigc/text-generation/generation <br/>
 * 不能使用 https://dashscope.aliyuncs.com/compatible-mode/v1 做为 Dashscope 的 baseUrl <br/>
 */
@Slf4j
public class DashScopeLLMService extends AbstractLLMService {

    public DashScopeLLMService(AiModel aiModel, ModelPlatform modelPlatform) {
        super(aiModel, modelPlatform);
    }

    @Override
    public boolean isEnabled() {
        return StringUtils.isNotBlank(platform.getApiKey()) && aiModel.getIsEnable();
    }

    @Override
    protected boolean checkBeforeChat(SseAskParams params) {
        if (CollectionUtils.isEmpty(params.getHttpRequestParams().getImageUrls()) && DashscopeUtil.vlChatModelNameProvider().anyMatch(item -> item.equalsIgnoreCase(params.getModelName()))) {
            log.warn("多模态LLM没有接收到图片,modelName:{}", params.getModelName());
        }
        return true;
    }

    @Override
    protected ChatModel doBuildChatModel(ChatModelBuilderProperties properties) {
        if (StringUtils.isBlank(platform.getApiKey())) {
            throw new BaseException(B_LLM_SECRET_KEY_NOT_SET);
        }
        String baseUrl = "";
        //OpenAI 兼容api格式不能用在 Dashscope 的 SDK 上
        if (!platform.getBaseUrl().contains("/compatible-mode")) {
            baseUrl = platform.getBaseUrl();
        }
        return QwenChatModel.builder()
                .apiKey(platform.getApiKey())
                .temperature(properties.getTemperature().floatValue())
                .modelName(aiModel.getName())
                .baseUrl(baseUrl)
                .build();
    }

    @Override
    public StreamingChatModel buildStreamingChatModel(ChatModelBuilderProperties properties) {
        if (StringUtils.isBlank(platform.getApiKey())) {
            throw new BaseException(B_LLM_SECRET_KEY_NOT_SET);
        }
        Double temperature = properties.getTemperatureWithDefault(0.7);
        String baseUrl = "";
        //OpenAI 兼容api格式不能用在 Dashscope 的 SDK 上
        if (!platform.getBaseUrl().contains("/compatible-mode")) {
            baseUrl = platform.getBaseUrl();
        }
        return QwenStreamingChatModel.builder()
                .apiKey(platform.getApiKey())
                .modelName(aiModel.getName())
                .temperature(temperature.floatValue())
                .baseUrl(baseUrl)
                .build();
    }

    @Override
    protected ChatRequestParameters doCreateChatRequestParameters(ChatRequestParameters defaultParameters, Map<String, Object> customParameters) {
        if (MapUtils.isEmpty(customParameters)) {
            return defaultParameters;
        }
        Boolean enableThinking = (Boolean) customParameters.get(ENABLE_THINKING);
        Boolean enableSearch = (Boolean) customParameters.get(ENABLE_WEB_SEARCH);
        QwenChatRequestParameters.Builder builder = QwenChatRequestParameters.builder();
        if (null != enableThinking) {
            builder.enableThinking(enableThinking);
        }
        if (null != enableSearch) {
            builder.enableSearch(enableSearch);
        }
        return builder.build().overrideWith(defaultParameters);
    }

    @Override
    public TokenCountEstimator getTokenEstimator() {
        if (aiModel.getName().contains("qwen-turbo") || aiModel.getName().contains("qwen-plus")) {
            return new QwenTokenCountEstimator(platform.getApiKey(), aiModel.getName());
        } else {
            return new OpenAiTokenCountEstimator(OpenAiChatModelName.GPT_3_5_TURBO);
        }
    }

    @Override
    protected LLMException parseError(Object error) {
        return null;
    }

}

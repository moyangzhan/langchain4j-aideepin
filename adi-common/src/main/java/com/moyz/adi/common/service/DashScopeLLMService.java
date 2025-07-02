package com.moyz.adi.common.service;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.interfaces.AbstractLLMService;
import com.moyz.adi.common.util.DashscopeUtil;
import com.moyz.adi.common.vo.DashScopeSetting;
import com.moyz.adi.common.vo.LLMBuilderProperties;
import com.moyz.adi.common.vo.LLMException;
import com.moyz.adi.common.vo.SseAskParams;
import dev.langchain4j.community.model.dashscope.QwenChatModel;
import dev.langchain4j.community.model.dashscope.QwenStreamingChatModel;
import dev.langchain4j.community.model.dashscope.QwenTokenCountEstimator;
import dev.langchain4j.model.TokenCountEstimator;
import dev.langchain4j.model.chat.ChatModel;
import dev.langchain4j.model.chat.StreamingChatModel;
import dev.langchain4j.model.openai.OpenAiChatModelName;
import dev.langchain4j.model.openai.OpenAiTokenCountEstimator;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;

import static com.moyz.adi.common.enums.ErrorEnum.B_LLM_SECRET_KEY_NOT_SET;

/**
 * 灵积模型服务(DashScope LLM service)
 */
@Slf4j
public class DashScopeLLMService extends AbstractLLMService<DashScopeSetting> {

    public DashScopeLLMService(AiModel aiModel) {
        super(aiModel, AdiConstant.SysConfigKey.DASHSCOPE_SETTING, DashScopeSetting.class);
    }

    @Override
    public boolean isEnabled() {
        return StringUtils.isNotBlank(modelPlatformSetting.getApiKey()) && aiModel.getIsEnable();
    }

    @Override
    protected boolean checkBeforeChat(SseAskParams params) {
        if (CollectionUtils.isEmpty(params.getChatModelParams().getImageUrls()) && DashscopeUtil.vlChatModelNameProvider().anyMatch(item -> item.equalsIgnoreCase(params.getModelName()))) {
            log.warn("多模态LLM没有接收到图片,modelName:{}", params.getModelName());
        }
        return true;
    }

    @Override
    protected ChatModel doBuildChatModel(LLMBuilderProperties properties) {
        if (StringUtils.isBlank(modelPlatformSetting.getApiKey())) {
            throw new BaseException(B_LLM_SECRET_KEY_NOT_SET);
        }
        return QwenChatModel.builder()
                .apiKey(modelPlatformSetting.getApiKey())
                .temperature(properties.getTemperature().floatValue())
                .modelName(aiModel.getName())
                .baseUrl(modelPlatformSetting.getBaseUrl())
                .build();
    }

    @Override
    public StreamingChatModel buildStreamingChatModel(LLMBuilderProperties properties) {
        if (StringUtils.isBlank(modelPlatformSetting.getApiKey())) {
            throw new BaseException(B_LLM_SECRET_KEY_NOT_SET);
        }
        Double temperature = properties.getTemperatureWithDefault(0.7);
        return QwenStreamingChatModel.builder()
                .apiKey(modelPlatformSetting.getApiKey())
                .modelName(aiModel.getName())
                .temperature(temperature.floatValue())
                .build();
    }

    @Override
    public TokenCountEstimator getTokenEstimator() {
        if (aiModel.getName().contains("qwen-turbo") || aiModel.getName().contains("qwen-plus")) {
            return new QwenTokenCountEstimator(modelPlatformSetting.getApiKey(), aiModel.getName());
        } else {
            return new OpenAiTokenCountEstimator(OpenAiChatModelName.GPT_3_5_TURBO);
        }
    }

    @Override
    protected LLMException parseError(Object error) {
        return null;
    }

}

package com.moyz.adi.common.service;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.interfaces.AbstractLLMService;
import com.moyz.adi.common.vo.DashScopeSetting;
import com.moyz.adi.common.vo.LLMBuilderProperties;
import dev.langchain4j.model.chat.ChatLanguageModel;
import dev.langchain4j.model.chat.StreamingChatLanguageModel;
import dev.langchain4j.model.dashscope.QwenChatModel;
import dev.langchain4j.model.dashscope.QwenStreamingChatModel;
import lombok.extern.slf4j.Slf4j;
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
    protected ChatLanguageModel buildChatLLM(LLMBuilderProperties properties) {
        if (StringUtils.isBlank(modelPlatformSetting.getApiKey())) {
            throw new BaseException(B_LLM_SECRET_KEY_NOT_SET);
        }
        float temperature = 0.7f;
        if (null != properties && properties.getTemperature() > 0 && properties.getTemperature() <= 1) {
            temperature = properties.getTemperature().floatValue();
        }
        return QwenChatModel.builder()
                .apiKey(modelPlatformSetting.getApiKey())
                .temperature(temperature)
                .modelName(aiModel.getName())
                .build();
    }

    @Override
    protected StreamingChatLanguageModel buildStreamingChatLLM(LLMBuilderProperties properties) {
        if (StringUtils.isBlank(modelPlatformSetting.getApiKey())) {
            throw new BaseException(B_LLM_SECRET_KEY_NOT_SET);
        }
        float temperature = 0.7f;
        if (null != properties && properties.getTemperature() > 0 && properties.getTemperature() <= 1) {
            temperature = properties.getTemperature().floatValue();
        }
        return QwenStreamingChatModel.builder()
                .apiKey(modelPlatformSetting.getApiKey())
                .modelName(aiModel.getName())
                .temperature(temperature)
                .build();
    }

    @Override
    protected String parseError(Object error) {
        return null;
    }

}

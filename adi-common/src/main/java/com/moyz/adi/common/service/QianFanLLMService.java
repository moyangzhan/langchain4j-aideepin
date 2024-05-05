package com.moyz.adi.common.service;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.interfaces.AbstractLLMService;
import com.moyz.adi.common.vo.QianFanSetting;
import dev.langchain4j.model.chat.ChatLanguageModel;
import dev.langchain4j.model.chat.StreamingChatLanguageModel;
import dev.langchain4j.model.qianfan.QianfanChatModel;
import dev.langchain4j.model.qianfan.QianfanStreamingChatModel;
import lombok.experimental.Accessors;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

/**
 * QianFan LLM service
 */
@Slf4j
@Accessors(chain = true)
public class QianFanLLMService extends AbstractLLMService<QianFanSetting> {

    public QianFanLLMService(AiModel aiModel) {
        super(aiModel, AdiConstant.SysConfigKey.QIANFAN_SETTING, QianFanSetting.class);
    }

    @Override
    public boolean isEnabled() {
        return StringUtils.isNoneBlank(modelPlatformSetting.getApiKey(), modelPlatformSetting.getSecretKey()) && aiModel.getIsEnable();
    }

    @Override
    protected ChatLanguageModel buildChatLLM() {
        return QianfanChatModel.builder()
                .modelName(aiModel.getName())
                .temperature(0.7)
                .topP(1.0)
                .maxRetries(1)
                .apiKey(modelPlatformSetting.getApiKey())
                .secretKey(modelPlatformSetting.getSecretKey())
                .build();
    }

    @Override
    protected StreamingChatLanguageModel buildStreamingChatLLM() {
        return QianfanStreamingChatModel.builder()
                .modelName(aiModel.getName())
                .temperature(0.7)
                .topP(1.0)
                .apiKey(modelPlatformSetting.getApiKey())
                .secretKey(modelPlatformSetting.getSecretKey())
                .build();
    }

    @Override
    protected String parseError(Object error) {
        return null;
    }
}

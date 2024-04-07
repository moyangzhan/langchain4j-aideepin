package com.moyz.adi.common.service;

import com.moyz.adi.common.cosntant.AdiConstant;
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

    public QianFanLLMService(String modelName) {
        super(modelName, AdiConstant.SysConfigKey.QIANFAN_SETTING, QianFanSetting.class);
    }

    @Override
    public boolean isEnabled() {
        return StringUtils.isNoneBlank(setting.getApiKey(), setting.getSecretKey());
    }

    @Override
    protected ChatLanguageModel buildChatLLM() {
        return QianfanChatModel.builder()
                .modelName(modelName)
                .temperature(0.7)
                .topP(1.0)
                .maxRetries(1)
                .apiKey(setting.getApiKey())
                .secretKey(setting.getSecretKey())
                .build();
    }

    @Override
    protected StreamingChatLanguageModel buildStreamingChatLLM() {
        return QianfanStreamingChatModel.builder()
                .modelName(modelName)
                .temperature(0.7)
                .topP(1.0)
                .apiKey(setting.getApiKey())
                .secretKey(setting.getSecretKey())
                .build();
    }

    @Override
    protected String parseError(Object error) {
        return null;
    }
}

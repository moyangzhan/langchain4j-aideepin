package com.moyz.adi.common.service;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.interfaces.AbstractLLMService;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.vo.QianFanAiModelSetting;
import com.moyz.adi.common.vo.QianFanAiPlatformSetting;
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
public class QianFanLLMService extends AbstractLLMService<QianFanAiPlatformSetting> {

    private String endPoint = "";

    public QianFanLLMService(AiModel aiModel) {
        super(aiModel, AdiConstant.SysConfigKey.QIANFAN_SETTING, QianFanAiPlatformSetting.class);
        endPoint = aiModel.getName();
        String ms = aiModel.getSetting();
        if (StringUtils.isNotBlank(ms)) {
            QianFanAiModelSetting aiModelSetting = JsonUtil.fromJson(ms, QianFanAiModelSetting.class);
            if (StringUtils.isNotBlank(aiModelSetting.getEndPoint())) {
                endPoint = aiModelSetting.getEndPoint();
            }
        }
    }

    @Override
    public boolean isEnabled() {
        return StringUtils.isNoneBlank(modelPlatformSetting.getApiKey(), modelPlatformSetting.getSecretKey()) && aiModel.getIsEnable();
    }

    @Override
    protected ChatLanguageModel buildChatLLM() {
        return QianfanChatModel.builder()
                .modelName(aiModel.getName())
                .endpoint(endPoint)
                .temperature(0.7)
                .topP(1.0)
                .maxRetries(1)
                .apiKey(modelPlatformSetting.getApiKey())
                .secretKey(modelPlatformSetting.getSecretKey())
                .logRequests(true)
                .logResponses(true)
                .build();
    }

    @Override
    protected StreamingChatLanguageModel buildStreamingChatLLM() {
        return QianfanStreamingChatModel.builder()
                .modelName(aiModel.getName())
                .endpoint(aiModel.getName())
                .temperature(0.7)
                .topP(1.0)
                .apiKey(modelPlatformSetting.getApiKey())
                .secretKey(modelPlatformSetting.getSecretKey())
                .logRequests(true)
                .logResponses(true)
                .build();
    }

    @Override
    protected String parseError(Object error) {
        return null;
    }
}

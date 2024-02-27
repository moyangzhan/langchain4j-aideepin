package com.moyz.adi.common.service;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.interfaces.AbstractLLMService;
import com.moyz.adi.common.vo.OpenAiSetting;
import dev.langchain4j.model.chat.ChatLanguageModel;
import dev.langchain4j.model.chat.StreamingChatLanguageModel;
import dev.langchain4j.model.openai.OpenAiChatModel;
import dev.langchain4j.model.openai.OpenAiStreamingChatModel;
import lombok.experimental.Accessors;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import java.net.Proxy;
import java.time.Duration;
import java.time.temporal.ChronoUnit;

/**
 * OpenAi LLM service
 */
@Slf4j
@Accessors(chain = true)
public class OpenAiLLMService extends AbstractLLMService<OpenAiSetting> {

    public OpenAiLLMService(String modelName, Proxy proxy) {
        super(modelName, AdiConstant.SysConfigKey.OPENAI_SETTING, OpenAiSetting.class, proxy);
    }

    @Override
    public boolean isEnabled() {
        return StringUtils.isNotBlank(setting.getSecretKey());
    }

    @Override
    protected StreamingChatLanguageModel buildStreamingChatLLM() {
        if (StringUtils.isBlank(setting.getSecretKey())) {
            throw new BaseException(ErrorEnum.B_LLM_SECRET_KEY_NOT_SET);
        }
        OpenAiStreamingChatModel.OpenAiStreamingChatModelBuilder builder = OpenAiStreamingChatModel
                .builder()
                .modelName(modelName)
                .apiKey(setting.getSecretKey())
                .timeout(Duration.of(60, ChronoUnit.SECONDS));
        if (null != proxy) {
            builder.proxy(proxy);
        }
        return builder.build();
    }

    @Override
    protected ChatLanguageModel buildChatLLM() {
        if (StringUtils.isBlank(setting.getSecretKey())) {
            throw new BaseException(ErrorEnum.B_LLM_SECRET_KEY_NOT_SET);
        }
        OpenAiChatModel.OpenAiChatModelBuilder builder = OpenAiChatModel.builder().apiKey(setting.getSecretKey());
        if (null != proxy) {
            builder.proxy(proxy);
        }
        return builder.build();
    }


}

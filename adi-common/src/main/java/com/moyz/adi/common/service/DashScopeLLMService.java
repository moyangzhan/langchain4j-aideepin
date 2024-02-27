package com.moyz.adi.common.service;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.interfaces.AbstractLLMService;
import com.moyz.adi.common.vo.DashScopeSetting;
import dev.langchain4j.model.chat.ChatLanguageModel;
import dev.langchain4j.model.chat.StreamingChatLanguageModel;
import dev.langchain4j.model.dashscope.QwenChatModel;
import dev.langchain4j.model.dashscope.QwenStreamingChatModel;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import java.net.Proxy;

import static com.moyz.adi.common.enums.ErrorEnum.B_LLM_SECRET_KEY_NOT_SET;

/**
 * 灵积模型服务(DashScope LLM service)
 */
@Slf4j
public class DashScopeLLMService extends AbstractLLMService<DashScopeSetting> {

    public DashScopeLLMService(String modelName, Proxy proxy) {
        super(modelName, AdiConstant.SysConfigKey.DASHSCOPE_SETTING, DashScopeSetting.class, proxy);
    }

    @Override
    public boolean isEnabled() {
        return StringUtils.isNotBlank(setting.getApiKey());
    }

    @Override
    protected StreamingChatLanguageModel buildStreamingChatLLM() {
        if (StringUtils.isBlank(setting.getApiKey())) {
            throw new BaseException(B_LLM_SECRET_KEY_NOT_SET);
        }
        return QwenStreamingChatModel.builder()
                .apiKey(setting.getApiKey())
                .modelName(modelName)
                .build();
    }

    @Override
    protected ChatLanguageModel buildChatLLM() {
        if (StringUtils.isBlank(setting.getApiKey())) {
            throw new BaseException(B_LLM_SECRET_KEY_NOT_SET);
        }
        return QwenChatModel.builder()
                .apiKey(setting.getApiKey())
                .modelName(modelName)
                .build();
    }

}

package com.moyz.adi.common.service.languagemodel;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.service.languagemodel.OpenAiLLMService;
import lombok.experimental.Accessors;
import lombok.extern.slf4j.Slf4j;

/**
 * DeepSeek API格式兼容OpenAi
 */
@Slf4j
@Accessors(chain = true)
public class DeepSeekLLMService extends OpenAiLLMService {
    public DeepSeekLLMService(AiModel model) {
        super(model, AdiConstant.SysConfigKey.DEEPSEEK_SETTING);
    }
}

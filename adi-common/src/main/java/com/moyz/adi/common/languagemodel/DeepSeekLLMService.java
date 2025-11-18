package com.moyz.adi.common.languagemodel;

import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.entity.ModelPlatform;
import lombok.experimental.Accessors;
import lombok.extern.slf4j.Slf4j;

/**
 * DeepSeek API格式兼容OpenAi
 */
@Slf4j
@Accessors(chain = true)
public class DeepSeekLLMService extends OpenAiLLMService {
    public DeepSeekLLMService(AiModel model, ModelPlatform modelPlatform) {
        super(model, modelPlatform);
    }
}

package com.moyz.adi.common.languagemodel;

import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.entity.ModelPlatform;


/**
 * 硅基流动 LLM服务
 *
 * @author pengh
 * @date 2025/04/16 14:05:35
 */
public class SiliconflowLLMService extends OpenAiLLMService {

    public SiliconflowLLMService(AiModel model, ModelPlatform modelPlatform) {
        super(model, modelPlatform);
    }
}

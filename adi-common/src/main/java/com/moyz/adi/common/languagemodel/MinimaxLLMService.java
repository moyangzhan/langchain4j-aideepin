package com.moyz.adi.common.languagemodel;

import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.entity.ModelPlatform;
import lombok.experimental.Accessors;
import lombok.extern.slf4j.Slf4j;

/**
 * MiniMax LLM service
 * MiniMax API is compatible with OpenAI API format
 * <a href="https://platform.minimaxi.com/document/introduction">MiniMax API Documentation</a>
 */
@Slf4j
@Accessors(chain = true)
public class MinimaxLLMService extends OpenAiLLMService {
    public MinimaxLLMService(AiModel model, ModelPlatform modelPlatform) {
        super(model, modelPlatform);
    }
}

package com.moyz.adi.common.service.languagemodel;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.AiModel;


/**
 * 硅基流动 LLM服务
 *
 * @author pengh
 * @date 2025/04/16 14:05:35
 */
public class SiliconflowLLMService extends OpenAiLLMService {

    public SiliconflowLLMService(AiModel aiModel) {
        super(aiModel, AdiConstant.SysConfigKey.SILICONFLOW_SETTING);
    }
}

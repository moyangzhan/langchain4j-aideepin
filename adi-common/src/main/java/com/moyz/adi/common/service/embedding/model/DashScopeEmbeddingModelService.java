package com.moyz.adi.common.service.embedding.model;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.interfaces.AbstractEmbeddingModelService;
import com.moyz.adi.common.vo.DashScopeSetting;
import dev.langchain4j.community.model.dashscope.QwenEmbeddingModel;
import dev.langchain4j.model.embedding.EmbeddingModel;
import lombok.experimental.Accessors;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import static com.moyz.adi.common.enums.ErrorEnum.B_LLM_SECRET_KEY_NOT_SET;

@Slf4j
@Accessors(chain = true)
public class DashScopeEmbeddingModelService extends AbstractEmbeddingModelService<DashScopeSetting> {

    public DashScopeEmbeddingModelService(AiModel model) {
        super(model, AdiConstant.SysConfigKey.DASHSCOPE_SETTING, DashScopeSetting.class);
    }

    @Override
    public EmbeddingModel buildModel() {
        if (StringUtils.isBlank(setting.getApiKey())) {
            throw new BaseException(B_LLM_SECRET_KEY_NOT_SET);
        }
        QwenEmbeddingModel.QwenEmbeddingModelBuilder builder = new QwenEmbeddingModel.QwenEmbeddingModelBuilder()
                .baseUrl(setting.getBaseUrl())
                .modelName(aiModel.getName())
                .apiKey(setting.getApiKey());
        if (StringUtils.isNotBlank(setting.getBaseUrl())) {
            builder.baseUrl(setting.getBaseUrl());
        }
        return builder.build();
    }
}

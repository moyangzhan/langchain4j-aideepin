package com.moyz.adi.common.service.languagemodel;

import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.entity.ModelPlatform;
import com.moyz.adi.common.exception.BaseException;
import dev.langchain4j.community.model.dashscope.QwenEmbeddingModel;
import dev.langchain4j.model.embedding.EmbeddingModel;
import lombok.experimental.Accessors;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import static com.moyz.adi.common.enums.ErrorEnum.B_LLM_SECRET_KEY_NOT_SET;

@Slf4j
@Accessors(chain = true)
public class DashScopeEmbeddingModelService extends AbstractEmbeddingModelService {

    public DashScopeEmbeddingModelService(AiModel model, ModelPlatform modelPlatform) {
        super(model, modelPlatform);
    }

    @Override
    public EmbeddingModel buildModel() {
        if (StringUtils.isBlank(platform.getApiKey())) {
            throw new BaseException(B_LLM_SECRET_KEY_NOT_SET);
        }
        QwenEmbeddingModel.QwenEmbeddingModelBuilder builder = new QwenEmbeddingModel.QwenEmbeddingModelBuilder()
                .baseUrl(platform.getBaseUrl())
                .modelName(aiModel.getName())
                .apiKey(platform.getApiKey());
        if (StringUtils.isNotBlank(platform.getBaseUrl())) {
            builder.baseUrl(platform.getBaseUrl());
        }
        return builder.build();
    }
}

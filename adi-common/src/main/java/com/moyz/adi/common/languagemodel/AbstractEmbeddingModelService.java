package com.moyz.adi.common.languagemodel;

import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.entity.ModelPlatform;
import dev.langchain4j.model.embedding.EmbeddingModel;

public abstract class AbstractEmbeddingModelService extends CommonModelService {

    protected AbstractEmbeddingModelService(AiModel model, ModelPlatform modelPlatform) {
        super(model, modelPlatform);
    }

    abstract public EmbeddingModel buildModel();
}

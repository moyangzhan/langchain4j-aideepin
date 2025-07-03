package com.moyz.adi.common.service.languagemodel;

import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.service.languagemodel.CommonModelService;
import dev.langchain4j.model.embedding.EmbeddingModel;

public abstract class AbstractEmbeddingModelService<T> extends CommonModelService<T> {

    protected AbstractEmbeddingModelService(AiModel aiModel, String settingName, Class<T> clazz) {
        super(aiModel, settingName, clazz);
    }

    abstract public EmbeddingModel buildModel();
}

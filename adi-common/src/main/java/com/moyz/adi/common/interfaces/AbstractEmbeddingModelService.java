package com.moyz.adi.common.interfaces;

import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.util.LocalCache;
import dev.langchain4j.model.embedding.EmbeddingModel;
import lombok.Getter;

import java.net.Proxy;

public abstract class AbstractEmbeddingModelService<T> {
    protected Proxy proxy;

    @Getter
    protected AiModel aiModel;

    protected T setting;

    protected AbstractEmbeddingModelService(AiModel aiModel, String settingName, Class<T> clazz) {
        this.aiModel = aiModel;
        String st = LocalCache.CONFIGS.get(settingName);
        setting = JsonUtil.fromJson(st, clazz);
    }

    abstract public EmbeddingModel buildModel();
}

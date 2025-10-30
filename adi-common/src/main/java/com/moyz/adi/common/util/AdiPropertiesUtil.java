package com.moyz.adi.common.util;

import com.fasterxml.jackson.databind.JsonNode;
import com.moyz.adi.common.config.AdiProperties;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.AiModel;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.tuple.Pair;

@Slf4j
public class AdiPropertiesUtil {

    public static String EMBEDDING_TABLE_SUFFIX = "";

    private AdiPropertiesUtil() {
    }

    public static Pair<String, Integer> getSuffixAndDimension(AdiProperties adiProperties) {
        String suffix = "";
        int dimension = 384;
        if (AdiConstant.EmbeddingModel.BGE_SMALL_ZH_V15.equals(adiProperties.getEmbeddingModel())) {
            dimension = AdiConstant.EmbeddingModel.BGE_SMALL_ZH_V15_DIMENSION;
            suffix = "bge_" + dimension;
            EMBEDDING_TABLE_SUFFIX = suffix;
        }
        //非本地向量模型
        else if (!AdiConstant.EmbeddingModel.LOCAL_MODELS.contains(adiProperties.getEmbeddingModel())) {
            AiModel aiModel = getEmbeddingModelByProperty(adiProperties);
            String platform = aiModel.getPlatform();
            String modelName = aiModel.getName();
            JsonNode jsonNode = aiModel.getProperties().get("dimension");
            if (null == jsonNode) {
                log.error("向量模型找不到定义的维度属性, model id:{}, model name:{}", aiModel.getId(), modelName);
                throw new RuntimeException("model dimension is not configured, model name:" + modelName);
            }
            dimension = jsonNode.asInt();
            suffix = platform + "_" + dimension;
            EMBEDDING_TABLE_SUFFIX = suffix;
        }
        Pair<String, Integer> tableSuffixAndDimension = Pair.of(suffix, dimension);
        log.info("getSuffixAndDimension:{}", tableSuffixAndDimension);
        return tableSuffixAndDimension;
    }

    public static AiModel getEmbeddingModelByProperty(AdiProperties adiProperties) {
        String[] platformAndModel = adiProperties.getEmbeddingModel().split(":");
        String platform = platformAndModel[0];
        String modelName = platformAndModel[1];
        AiModel aiModel = LocalCache.MODEL_ID_TO_OBJ.values().stream()
                .filter(item -> item.getPlatform().equals(platform) && item.getName().equals(modelName))
                .findFirst()
                .orElse(null);
        if (null == aiModel) {
            log.error("模型找不到或已被禁用,platform:{},name:{}", platform, modelName);
            throw new RuntimeException("模型找不到或已被禁用 | vector model not found or is disabled,name:" + modelName);
        }
        return aiModel;
    }
}

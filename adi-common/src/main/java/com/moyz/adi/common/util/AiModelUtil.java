package com.moyz.adi.common.util;

import com.moyz.adi.common.cosntant.AdiConstant;
import org.apache.commons.lang3.StringUtils;

public class AiModelUtil {

    private AiModelUtil() {
    }

    public static boolean checkModelType(String modelType) {
        return StringUtils.equalsAny(modelType, AdiConstant.ModelType.TEXT, AdiConstant.ModelType.IMAGE, AdiConstant.ModelType.EMBEDDING, AdiConstant.ModelType.RERANK);
    }

    public static boolean checkModelPlatform(String platform) {
        return AdiConstant.ModelPlatform.getModelConstants().contains(platform);
    }
}

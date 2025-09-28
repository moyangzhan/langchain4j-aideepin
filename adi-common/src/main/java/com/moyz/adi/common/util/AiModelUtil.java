package com.moyz.adi.common.util;

import com.moyz.adi.common.cosntant.AdiConstant;
import org.apache.commons.lang3.StringUtils;

public class AiModelUtil {

    private AiModelUtil() {
    }

    public static boolean checkModelType(String modelType) {
        return AdiConstant.ModelType.getModelType().contains(modelType);
    }

    public static boolean checkModelPlatform(String platform) {
        return AdiConstant.ModelPlatform.getModelConstants().contains(platform);
    }
}

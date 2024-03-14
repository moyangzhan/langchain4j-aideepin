package com.moyz.adi.common.helper;

import com.moyz.adi.common.interfaces.AbstractImageModelService;
import com.moyz.adi.common.vo.ImageModelInfo;
import lombok.extern.slf4j.Slf4j;

import java.util.HashMap;
import java.util.Map;

import static dev.langchain4j.model.openai.OpenAiModelName.DALL_E_2;

/**
 * image model service上下文类（策略模式）
 */
@Slf4j
public class ImageModelContext {

    /**
     * AI图片模型
     */
    public static final Map<String, ImageModelInfo> NAME_TO_MODEL = new HashMap<>();

    private AbstractImageModelService modelService;

    public ImageModelContext() {
        modelService = NAME_TO_MODEL.get(DALL_E_2).getModelService();
    }

    public ImageModelContext(String modelName) {
        if (null == NAME_TO_MODEL.get(modelName)) {
            log.warn("︿︿︿ Can not find {}, use the default model DALL_E_2 ︿︿︿", modelName);
            modelService = NAME_TO_MODEL.get(DALL_E_2).getModelService();
        } else {
            modelService = NAME_TO_MODEL.get(modelName).getModelService();
        }
    }

    public static void addImageModelService(String modelServiceKey, AbstractImageModelService modelService) {
        ImageModelInfo imageModelInfo = new ImageModelInfo();
        imageModelInfo.setModelService(modelService);
        imageModelInfo.setModelName(modelServiceKey);
        imageModelInfo.setEnable(modelService.isEnabled());
        NAME_TO_MODEL.put(modelServiceKey, imageModelInfo);
    }

    public AbstractImageModelService getModelService() {
        return modelService;
    }
}

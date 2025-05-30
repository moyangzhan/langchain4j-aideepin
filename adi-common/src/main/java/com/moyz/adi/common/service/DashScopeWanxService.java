package com.moyz.adi.common.service;

import com.alibaba.dashscope.exception.ApiException;
import com.fasterxml.jackson.databind.JsonNode;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.entity.Draw;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.interfaces.AbstractImageModelService;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.util.wanx.AdiWanxImageModel;
import com.moyz.adi.common.vo.DashScopeSetting;
import com.moyz.adi.common.vo.LLMException;
import com.moyz.adi.common.vo.WanxBackgroundGenerationParams;
import dev.langchain4j.model.image.ImageModel;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import java.util.List;

/**
 * 通义万相
 */
@Slf4j
public class DashScopeWanxService extends AbstractImageModelService<DashScopeSetting> {

    public DashScopeWanxService(AiModel aiModel) {
        super(aiModel, AdiConstant.SysConfigKey.DASHSCOPE_SETTING, DashScopeSetting.class);
    }

    @Override
    public boolean isEnabled() {
        return StringUtils.isNotBlank(setting.getApiKey()) && aiModel.getIsEnable();
    }

    @Override
    protected ImageModel buildImageModel(User user, Draw draw) {
        if (draw.getAiModelName().contains("wanx-background-generation")) {
            JsonNode dynamicParams = draw.getDynamicParams();
            if (dynamicParams.isEmpty()) {
                log.error("动态参数不能为空");
                throw new BaseException(ErrorEnum.A_PARAMS_ERROR);
            }
            WanxBackgroundGenerationParams dynamicParamsObj = JsonUtil.fromJson(dynamicParams, WanxBackgroundGenerationParams.class);
            if (null == dynamicParamsObj) {
                log.error("动态参数解析失败");
                throw new BaseException(ErrorEnum.A_PARAMS_ERROR);
            }
            if (StringUtils.isAllBlank(dynamicParamsObj.getRefImageUrl(), dynamicParamsObj.getRefPrompt())) {
                log.error("引导图与提示词不能全部为空,dynamicParams:{}", dynamicParamsObj);
                throw new BaseException(ErrorEnum.A_PARAMS_ERROR);
            }
            AdiWanxImageModel model = AdiWanxImageModel.builder()
                    .baseUrl(setting.getBaseUrl())
                    .apiKey(setting.getApiKey())
                    .modelName(draw.getAiModelName())
                    .task("background-generation")
                    .function("generation")
                    .size(draw.getGenerateSize())
                    .build();
            model.setImageSynthesisParamCustomizer((paramBuilder -> {
                paramBuilder.extraInput("base_image_url", dynamicParamsObj.getBaseImageUrl());
                if (StringUtils.isNotBlank(dynamicParamsObj.getRefImageUrl())) {
                    paramBuilder.extraInput("ref_image_url", dynamicParamsObj.getRefImageUrl());
                }
                if (StringUtils.isNotBlank(dynamicParamsObj.getRefPrompt())) {
                    paramBuilder.extraInput("ref_prompt", dynamicParamsObj.getRefPrompt());
                }
                paramBuilder.parameter("model_version", "v3");
            }));
            return model;
        } else {
            AdiWanxImageModel.WanxImageModelBuilder builder = AdiWanxImageModel.builder()
                    .baseUrl(setting.getBaseUrl())
                    .apiKey(setting.getApiKey())
                    .modelName(draw.getAiModelName())
                    .size(draw.getGenerateSize())
                    .negativePrompt(draw.getNegativePrompt());
            if (null != draw.getGenerateSeed() && draw.getGenerateSeed() > 0) {
                builder.seed(draw.getGenerateSeed());
            }
            return builder.build();
        }

    }

    @Override
    public List<String> editImage(User user, Draw draw) {
        //待实现
        throw new IllegalArgumentException("Operation is not supported");
    }

    @Override
    public List<String> createImageVariation(User user, Draw draw) {
        //待实现
        throw new IllegalArgumentException("Operation is not supported");
    }

    @Override
    protected LLMException parseError(Object error) {
        if (error instanceof ApiException apiException) {
            LLMException llmException = new LLMException();
            llmException.setType(apiException.getStatus().getCode());
            llmException.setCode(apiException.getStatus().getCode());
            llmException.setMessage(apiException.getStatus().getMessage());
            return llmException;
        }
        return null;
    }
}

package com.moyz.adi.common.service;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.interfaces.AbstractLLMService;
import com.moyz.adi.common.interfaces.TriConsumer;
import com.moyz.adi.common.util.DashscopeUtil;
import com.moyz.adi.common.vo.*;
import dev.langchain4j.data.image.Image;
import dev.langchain4j.data.message.ChatMessage;
import dev.langchain4j.data.message.ImageContent;
import dev.langchain4j.data.message.TextContent;
import dev.langchain4j.data.message.UserMessage;
import dev.langchain4j.model.chat.ChatLanguageModel;
import dev.langchain4j.model.chat.StreamingChatLanguageModel;
import dev.langchain4j.model.dashscope.QwenChatModel;
import dev.langchain4j.model.dashscope.QwenStreamingChatModel;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Base64;
import java.util.Collections;
import java.util.List;

import static com.moyz.adi.common.enums.ErrorEnum.B_LLM_SECRET_KEY_NOT_SET;

/**
 * 灵积模型服务(DashScope LLM service)
 */
@Slf4j
public class DashScopeLLMService extends AbstractLLMService<DashScopeSetting> {

    public DashScopeLLMService(AiModel aiModel) {
        super(aiModel, AdiConstant.SysConfigKey.DASHSCOPE_SETTING, DashScopeSetting.class);
    }

    @Override
    public boolean isEnabled() {
        return StringUtils.isNotBlank(modelPlatformSetting.getApiKey()) && aiModel.getIsEnable();
    }

    @Override
    protected boolean checkBeforeChat(SseAskParams params) {
        if (CollectionUtils.isEmpty(params.getAssistantChatParams().getImageUrls()) && DashscopeUtil.vlChatModelNameProvider().anyMatch(item -> item.equalsIgnoreCase(params.getModelName()))) {
            log.warn("多模态LLM没有接收到图片,modelName:{}", params.getModelName());
        }
        return true;
    }

    @Override
    protected ChatLanguageModel doBuildChatLLM(LLMBuilderProperties properties) {
        if (StringUtils.isBlank(modelPlatformSetting.getApiKey())) {
            throw new BaseException(B_LLM_SECRET_KEY_NOT_SET);
        }
        float temperature = 0.7f;
        if (null != properties && properties.getTemperature() > 0 && properties.getTemperature() <= 1) {
            temperature = properties.getTemperature().floatValue();
        }
        return QwenChatModel.builder()
                .apiKey(modelPlatformSetting.getApiKey())
                .temperature(temperature)
                .modelName(aiModel.getName())
                .build();
    }

    @Override
    public StreamingChatLanguageModel buildStreamingChatLLM(LLMBuilderProperties properties) {
        if (StringUtils.isBlank(modelPlatformSetting.getApiKey())) {
            throw new BaseException(B_LLM_SECRET_KEY_NOT_SET);
        }
        float temperature = 0.7f;
        if (null != properties && properties.getTemperature() > 0 && properties.getTemperature() <= 1) {
            temperature = properties.getTemperature().floatValue();
        }
        return QwenStreamingChatModel.builder()
                .apiKey(modelPlatformSetting.getApiKey())
                .modelName(aiModel.getName())
                .temperature(temperature)
                .build();
    }

    //    public static List<ChatMessage> multiModalChat() {
//        Image image = Image.builder()
//                .base64Data(multimodalImageData())
//                .mimeType("image/jpeg")
//                .build();
//        ImageContent imageContent = ImageContent.from(image);
//        TextContent textContent = TextContent.from("What animal is in the picture?");
//        return Collections.singletonList(UserMessage.from(imageContent, textContent));
//    }

    @Override
    protected String parseError(Object error) {
        return null;
    }

}

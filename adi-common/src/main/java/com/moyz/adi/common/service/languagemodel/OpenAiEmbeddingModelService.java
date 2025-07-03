package com.moyz.adi.common.service.languagemodel;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.vo.OpenAiSetting;
import dev.langchain4j.http.client.jdk.JdkHttpClient;
import dev.langchain4j.model.embedding.EmbeddingModel;
import dev.langchain4j.model.openai.OpenAiEmbeddingModel;
import lombok.experimental.Accessors;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import java.net.ProxySelector;
import java.net.http.HttpClient;

@Slf4j
@Accessors(chain = true)
public class OpenAiEmbeddingModelService extends AbstractEmbeddingModelService<OpenAiSetting> {

    public OpenAiEmbeddingModelService(AiModel model) {
        super(model, AdiConstant.SysConfigKey.OPENAI_SETTING, OpenAiSetting.class);
    }

    @Override
    public EmbeddingModel buildModel() {
        if (StringUtils.isBlank(platformSetting.getSecretKey())) {
            throw new BaseException(ErrorEnum.B_LLM_SECRET_KEY_NOT_SET);
        }
        OpenAiEmbeddingModel.OpenAiEmbeddingModelBuilder builder = new OpenAiEmbeddingModel.OpenAiEmbeddingModelBuilder()
                .baseUrl(platformSetting.getBaseUrl())
                .modelName(aiModel.getName())
                .dimensions(aiModel.getProperties().get("dimension").asInt())
                .apiKey(platformSetting.getSecretKey());
        if (StringUtils.isNotBlank(platformSetting.getBaseUrl())) {
            builder.baseUrl(platformSetting.getBaseUrl());
        }
        if (null != proxyAddress) {
            HttpClient.Builder httpClientBuilder = HttpClient.newBuilder().proxy(ProxySelector.of(proxyAddress));
            builder.httpClientBuilder(JdkHttpClient.builder().httpClientBuilder(httpClientBuilder));
        }
        return builder.build();
    }
}

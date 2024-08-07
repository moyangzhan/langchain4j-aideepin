package com.moyz.adi.common.interfaces;

import com.moyz.adi.common.entity.AiImage;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.util.LocalCache;
import com.moyz.adi.common.vo.ImageModelBuilderProperties;
import dev.langchain4j.data.image.Image;
import dev.langchain4j.model.image.ImageModel;
import dev.langchain4j.model.output.Response;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;

import java.net.Proxy;
import java.util.Collections;
import java.util.List;

@Slf4j
public abstract class AbstractImageModelService<T> {

    protected Proxy proxy;

    protected AiModel aiModel;

    protected T setting;

    @Value("${adi.proxy.enable:false}")
    protected boolean proxyEnable;

    @Value("${adi.proxy.host:0}")
    protected String proxyHost;

    @Value("${adi.proxy.http-port:0}")
    protected int proxyHttpPort;

    protected ImageModel imageModel;

    protected AbstractImageModelService(AiModel aiModel, String settingName, Class<T> clazz) {
        this.aiModel = aiModel;
        String st = LocalCache.CONFIGS.get(settingName);
        setting = JsonUtil.fromJson(st, clazz);
    }

    public AbstractImageModelService<T> setProxy(Proxy proxy) {
        this.proxy = proxy;
        return this;
    }

    public ImageModel getImageModel(User user, ImageModelBuilderProperties builderProperties) {
        if (null != imageModel) {
            return imageModel;
        }
        imageModel = buildImageModel(user, builderProperties);
        return imageModel;
    }

    /**
     * 检测该service是否可用（不可用的情况通过是没有配置key）
     *
     * @return
     */
    public abstract boolean isEnabled();

    protected abstract ImageModel buildImageModel(User user, ImageModelBuilderProperties builderProperties);

    public List<String> generateImage(User user, AiImage aiImage) {
        ImageModelBuilderProperties builderProperties = ImageModelBuilderProperties.builder()
                .size(aiImage.getGenerateSize())
                .quality(aiImage.getGenerateQuality())
                .build();
        ImageModel curImageModel = getImageModel(user, builderProperties);
        try {
            Response<List<Image>> response = curImageModel.generate(aiImage.getPrompt(), aiImage.getGenerateNumber());
            log.info("createImage response:{}", response);
            return response.content().stream().map(item -> item.url().toString()).toList();
        } catch (Exception e) {
            log.error("create image error", e);
        }
        return Collections.emptyList();
    }

    /**
     * DALL·E 2 only
     *
     * @param user
     * @param aiImage
     * @return
     */
    public abstract List<String> editImage(User user, AiImage aiImage);

    /**
     * DALL·E 2 only
     *
     * @param user
     * @param aiImage
     * @return
     */
    public abstract List<String> createImageVariation(User user, AiImage aiImage);

    public AiModel getAiModel() {
        return aiModel;
    }
}

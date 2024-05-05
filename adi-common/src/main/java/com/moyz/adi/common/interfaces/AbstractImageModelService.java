package com.moyz.adi.common.interfaces;

import com.moyz.adi.common.entity.AiImage;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.util.LocalCache;
import dev.langchain4j.data.image.Image;
import dev.langchain4j.model.image.ImageModel;
import dev.langchain4j.model.output.Response;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;

import java.net.Proxy;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import static com.moyz.adi.common.cosntant.AdiConstant.OPENAI_CREATE_IMAGE_SIZES;

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

    public AbstractImageModelService(AiModel aiModel, String settingName, Class<T> clazz) {
        this.aiModel = aiModel;
        String st = LocalCache.CONFIGS.get(settingName);
        setting = JsonUtil.fromJson(st, clazz);
    }

    public AbstractImageModelService setProxy(Proxy proxy) {
        this.proxy = proxy;
        return this;
    }

    public ImageModel getImageModel(User user, String size) {
        if (null != imageModel) {
            return imageModel;
        }
        imageModel = buildImageModel(user, size);
        return imageModel;
    }

    /**
     * 检测该service是否可用（不可用的情况通过是没有配置key）
     *
     * @return
     */
    public abstract boolean isEnabled();

    protected abstract ImageModel buildImageModel(User user, String size);

    public List<String> createImage(User user, AiImage aiImage) {
        if (aiImage.getGenerateNumber() < 1 || aiImage.getGenerateNumber() > 10) {
            throw new BaseException(ErrorEnum.A_IMAGE_NUMBER_ERROR);
        }
        if (!OPENAI_CREATE_IMAGE_SIZES.contains(aiImage.getGenerateSize())) {
            throw new BaseException(ErrorEnum.A_IMAGE_SIZE_ERROR);
        }
        ImageModel imageModel = getImageModel(user, aiImage.getGenerateSize());
        try {
            Response<List<Image>> response = imageModel.generate(aiImage.getPrompt(), aiImage.getGenerateNumber());
            log.info("createImage response:{}", response);
            return response.content().stream().map(item -> item.url().toString()).collect(Collectors.toList());
        } catch (Exception e) {
            log.error("create image error", e);
        }
        return Collections.emptyList();
    }

    public abstract List<String> editImage(User user, AiImage aiImage);

    public abstract List<String> createImageVariation(User user, AiImage aiImage);

    public AiModel getAiModel() {
        return aiModel;
    }
}

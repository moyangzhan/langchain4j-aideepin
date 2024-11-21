package com.moyz.adi.common.interfaces;

import com.moyz.adi.common.entity.Draw;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.service.FileService;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.util.LocalCache;
import com.moyz.adi.common.util.SpringUtil;
import com.moyz.adi.common.vo.ImageModelBuilderProperties;
import com.moyz.adi.common.vo.LLMException;
import dev.langchain4j.data.image.Image;
import dev.langchain4j.model.image.ImageModel;
import dev.langchain4j.model.output.Response;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;

import java.net.Proxy;
import java.util.Collections;
import java.util.List;

import static com.moyz.adi.common.enums.ErrorEnum.C_DRAW_FAIL;

@Slf4j
public abstract class AbstractImageModelService<T> {

    protected Proxy proxy;

    @Getter
    protected AiModel aiModel;

    protected T setting;

    private FileService fileService;

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

    public List<String> generateImage(User user, Draw draw) {
        ImageModelBuilderProperties builderProperties = ImageModelBuilderProperties.builder()
                .size(draw.getGenerateSize())
                .quality(draw.getGenerateQuality())
                .build();
        ImageModel curImageModel = getImageModel(user, builderProperties);
        try {
            Response<List<Image>> response = curImageModel.generate(draw.getPrompt(), draw.getGenerateNumber());
            log.info("createImage response:{}", response);
            return response.content().stream().map(item -> item.url().toString()).toList();
        } catch (Exception e) {
            log.error("create image error", e);
            LLMException llmException = parseError(e);
            if (null != llmException) {
                throw new BaseException(C_DRAW_FAIL, llmException.getMessage());
            }
            throw new BaseException(C_DRAW_FAIL, e.getMessage());
        }
    }

    /**
     * DALL·E 2 only
     *
     * @param user
     * @param draw
     * @return
     */
    public abstract List<String> editImage(User user, Draw draw);

    /**
     * DALL·E 2 only
     *
     * @param user
     * @param draw
     * @return
     */
    public abstract List<String> createImageVariation(User user, Draw draw);

    protected abstract LLMException parseError(Object error);

    public FileService getFileService() {
        if (null == fileService) {
            fileService = SpringUtil.getBean(FileService.class);
        }
        return fileService;
    }
}

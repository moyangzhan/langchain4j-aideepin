package com.moyz.adi.common.service.languagemodel;

import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.entity.Draw;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.service.FileService;
import com.moyz.adi.common.util.SpringUtil;
import com.moyz.adi.common.vo.LLMException;
import dev.langchain4j.data.image.Image;
import dev.langchain4j.model.image.ImageModel;
import dev.langchain4j.model.output.Response;
import lombok.extern.slf4j.Slf4j;

import java.net.InetSocketAddress;
import java.util.List;

import static com.moyz.adi.common.enums.ErrorEnum.C_DRAW_FAIL;

@Slf4j
public abstract class AbstractImageModelService<T> extends CommonModelService<T> {

    private FileService fileService;

    protected AbstractImageModelService(AiModel aiModel, String settingName, Class<T> clazz) {
        super(aiModel, settingName, clazz);
    }

    public AbstractImageModelService<T> setProxyAddress(InetSocketAddress proxyAddress) {
        this.proxyAddress = proxyAddress;
        return this;
    }

    public ImageModel getImageModel(User user, Draw draw) {
        return buildImageModel(user, draw);
    }

    /**
     * 检测该service是否可用（不可用的情况通过是没有配置key）
     *
     * @return
     */
    public abstract boolean isEnabled();

    protected abstract ImageModel buildImageModel(User user, Draw draw);

    public List<String> generateImage(User user, Draw draw) {
        ImageModel curImageModel = getImageModel(user, draw);
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

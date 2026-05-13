package com.moyz.adi.common.languagemodel;

import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.entity.Draw;
import com.moyz.adi.common.entity.ModelPlatform;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.service.FileService;
import com.moyz.adi.common.file.LocalFileOperator;
import com.moyz.adi.common.file.LocalFileUtil;
import com.moyz.adi.common.util.SpringUtil;
import com.moyz.adi.common.util.UuidUtil;
import com.moyz.adi.common.languagemodel.data.LLMException;
import dev.langchain4j.data.image.Image;
import dev.langchain4j.model.image.ImageModel;
import dev.langchain4j.model.output.Response;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.tuple.Pair;

import java.io.File;
import java.net.InetSocketAddress;
import java.util.Base64;
import java.util.List;
import java.util.Objects;

import static com.moyz.adi.common.enums.ErrorEnum.C_DRAW_FAIL;

@Slf4j
public abstract class AbstractImageModelService extends CommonModelService {

    private FileService fileService;

    protected AbstractImageModelService(AiModel model, ModelPlatform modelPlatform) {
        super(model, modelPlatform);
    }

    public AbstractImageModelService setProxyAddress(InetSocketAddress proxyAddress) {
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
            return response.content().stream().map(item -> {
                if (item.url() != null) {
                    return item.url().toString();
                }
                if (item.base64Data() != null) {
                    return saveBase64ToTempFile(item.base64Data());
                }
                return null;
            }).filter(Objects::nonNull).toList();
        } catch (Exception e) {
            log.error("create image error", e);
            LLMException llmException = parseError(e);
            if (null != llmException) {
                throw new BaseException(C_DRAW_FAIL, llmException.getMessage());
            }
            throw new BaseException(C_DRAW_FAIL, e.getMessage());
        }
    }

    private String saveBase64ToTempFile(String base64Data) {
        byte[] imageBytes = Base64.getDecoder().decode(base64Data);
        String uuid = UuidUtil.createShort();
        Pair<String, String> saved = LocalFileUtil.saveToLocal(imageBytes, LocalFileOperator.tmpImagePath, "adi_" + uuid + ".png");
        return new File(saved.getLeft()).toURI().toString();
    }

    protected abstract LLMException parseError(Object error);

    public FileService getFileService() {
        if (null == fileService) {
            fileService = SpringUtil.getBean(FileService.class);
        }
        return fileService;
    }
}

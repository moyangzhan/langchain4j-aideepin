package com.moyz.adi.common.languagemodel;

import com.alibaba.dashscope.exception.ApiException;
import com.moyz.adi.common.entity.AdiFile;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.entity.Draw;
import com.moyz.adi.common.entity.ModelPlatform;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.languagemodel.data.LLMException;
import com.moyz.adi.common.service.FileService;
import com.moyz.adi.common.util.ImageUtil;
import dev.langchain4j.model.image.ImageModel;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.Base64;
import java.util.List;

/**
 * DashScope 图片生成(wan2.7-image / qwen-image-3.0)。
 * <p>
 * 全部模型统一走新一代 image-generation 接口:文生图与图生图同端点;
 * 图生图参考图以 base64 data URI 由服务端直传,本地文件存储与 OSS 存储均可用,不依赖文件公网可访问。
 */
@Slf4j
public class DashScopeImageService extends AbstractImageModelService {

    public DashScopeImageService(AiModel model, ModelPlatform modelPlatform) {
        super(model, modelPlatform);
    }

    @Override
    public boolean isEnabled() {
        return StringUtils.isNotBlank(platform.getApiKey()) && aiModel.getIsEnable();
    }

    @Override
    protected ImageModel buildImageModel(User user, Draw draw) {
        List<String> allowedSizes = aiModel.getSizes();
        if (!allowedSizes.isEmpty() && !allowedSizes.contains(draw.getGenerateSize())) {
            log.warn("Invalid size {} for model {}, allowed: {}",
                    draw.getGenerateSize(), draw.getAiModelName(), allowedSizes);
            throw new BaseException(ErrorEnum.A_PARAMS_ERROR);
        }
        AdiImageGenerationModel.AdiImageGenerationModelBuilder builder = AdiImageGenerationModel.builder()
                .apiKey(platform.getApiKey())
                .modelName(draw.getAiModelName())
                .size(draw.getGenerateSize())
                .baseUrl(platform.getBaseUrl());
        if (null != draw.getGenerateSeed() && draw.getGenerateSeed() > 0) {
            builder.seed(draw.getGenerateSeed());
        }
        List<String> refImageUuids = draw.getRefImageUuids();
        if (!refImageUuids.isEmpty()) {
            List<String> dataUrls = new ArrayList<>(refImageUuids.size());
            for (String uuid : refImageUuids) {
                dataUrls.add(toImageDataUrl(uuid));
            }
            builder.refImageDataUrls(dataUrls);
        }
        return builder.build();
    }

    /**
     * 参考图文件转 base64 data URI:本地存储读盘、OSS 从可访问 URL 下载,均由服务端直传模型服务
     */
    private String toImageDataUrl(String uuid) {
        FileService fileService = getFileService();
        AdiFile adiFile = fileService.getFile(uuid);
        byte[] bytes = fileService.readFileBytes(adiFile);
        log.info("Ref image ready, uuid:{}, bytes:{}", uuid, bytes.length);
        return "data:" + ImageUtil.mimeTypeByExt(adiFile.getExt()) + ";base64,"
                + Base64.getEncoder().encodeToString(bytes);
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

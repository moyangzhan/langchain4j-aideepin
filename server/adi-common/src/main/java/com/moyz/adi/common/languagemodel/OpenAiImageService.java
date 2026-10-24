package com.moyz.adi.common.languagemodel;

import com.moyz.adi.common.entity.*;
import com.moyz.adi.common.entity.ModelPlatform;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.languagemodel.data.LLMException;
import com.moyz.adi.common.service.FileService;
import com.moyz.adi.common.util.ImageUtil;
import dev.langchain4j.http.client.jdk.JdkHttpClient;
import dev.langchain4j.model.image.ImageModel;
import dev.langchain4j.model.openai.OpenAiImageModel;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import java.net.ProxySelector;
import java.net.http.HttpClient;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import static com.moyz.adi.common.cosntant.AdiConstant.*;

@Slf4j
public class OpenAiImageService extends AbstractImageModelService {

    public OpenAiImageService(AiModel model, ModelPlatform modelPlatform) {
        super(model, modelPlatform);
    }

    @Override
    public boolean isEnabled() {
        return StringUtils.isNotBlank(platform.getApiKey()) && aiModel.getIsEnable();
    }

    @Override
    public ImageModel buildImageModel(User user, Draw draw) {
        if (StringUtils.isBlank(platform.getApiKey())) {
            throw new BaseException(ErrorEnum.B_LLM_SECRET_KEY_NOT_SET);
        }
        List<String> refImageUuids = draw.getRefImageUuids();
        if (!refImageUuids.isEmpty()) {
            // 图生图:langchain4j OpenAiImageModel 不支持 edits 端点,走自研 OpenAiImageEditModel
            FileService fileService = getFileService();
            List<OpenAiImageEditModel.RefImage> refImages = new ArrayList<>(refImageUuids.size());
            for (String uuid : refImageUuids) {
                AdiFile adiFile = fileService.getFile(uuid);
                refImages.add(new OpenAiImageEditModel.RefImage(
                        fileService.readFileBytes(adiFile),
                        ImageUtil.mimeTypeByExt(adiFile.getExt()),
                        "ref-" + uuid + "." + adiFile.getExt()));
            }
            return OpenAiImageEditModel.builder()
                    .baseUrl(platform.getBaseUrl())
                    .apiKey(platform.getApiKey())
                    .modelName(aiModel.getName())
                    .size(draw.getGenerateSize())
                    .quality(draw.getGenerateQuality())
                    .refImages(refImages)
                    .proxyAddress(Boolean.TRUE.equals(platform.getIsProxyEnable()) ? proxyAddress : null)
                    .build();
        }
        OpenAiImageModel.OpenAiImageModelBuilder builder = OpenAiImageModel.builder()
                .baseUrl(platform.getBaseUrl())
                .modelName(aiModel.getName())
                .apiKey(platform.getApiKey())
                .user(user.getUuid())
                .size(Objects.toString(draw.getGenerateSize(), "1024x1024"))
                .quality(Objects.toString(draw.getGenerateQuality(), "medium"))
                .logRequests(true)
                .logResponses(true)
                .maxRetries(1);
        if (null != proxyAddress && platform.getIsProxyEnable()) {
            HttpClient.Builder httpClientBuilder = HttpClient.newBuilder().proxy(ProxySelector.of(proxyAddress));
            builder.httpClientBuilder(JdkHttpClient.builder().httpClientBuilder(httpClientBuilder));
        }
        return builder.build();
    }

    @Override
    protected LLMException parseError(Object error) {
        if (error instanceof Exception e) {
            LLMException llmException = new LLMException();
            llmException.setMessage(e.getMessage());
            return llmException;
        }
        return null;
    }
}

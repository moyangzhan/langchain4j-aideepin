package com.moyz.adi.common.languagemodel;

import com.moyz.adi.common.entity.AdiFile;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.entity.Draw;
import com.moyz.adi.common.entity.ModelPlatform;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.languagemodel.data.LLMException;
import com.moyz.adi.common.service.FileService;
import com.moyz.adi.common.util.ImageUtil;
import dev.langchain4j.http.client.jdk.JdkHttpClient;
import dev.langchain4j.model.image.ImageModel;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import java.net.ProxySelector;
import java.net.http.HttpClient;
import java.util.Base64;
import java.util.List;

/**
 * 硅基流动 图像模型服务
 */
@Slf4j
public class SiliconflowImageModelService extends AbstractImageModelService {

    public SiliconflowImageModelService(AiModel model, ModelPlatform modelPlatform) {
        super(model, modelPlatform);
    }

    @Override
    public boolean isEnabled() {
        return StringUtils.isNotBlank(platform.getApiKey()) && aiModel.getIsEnable();
    }

    @Override
    protected ImageModel buildImageModel(User user, Draw draw) {
        SiliconflowImageModel.SiliconflowImageModelBuilder builder = SiliconflowImageModel.builder()
                .baseUrl(platform.getBaseUrl())
                .apiKey(platform.getApiKey())
                .modelName(draw.getAiModelName())
                .imageSize(draw.getGenerateSize())
                .negativePrompt(draw.getNegativePrompt());
        // seed 合法范围为 0-9999999999;随机(-1)或未指定时不发送该字段,
        // 否则硅基流动报 20015 "seed should be a integer"(2026-10-23 图生图实测触发)
        // seed's valid range is 0-9999999999; omit it for random (-1) or unset,
        // otherwise SiliconFlow rejects with 20015 (hit in i2i testing on 2026-10-23)
        if (null != draw.getGenerateSeed() && draw.getGenerateSeed() > 0) {
            builder.seed(draw.getGenerateSeed().longValue());
        }
        // 图生图:Kolors 的 image 参数为单图(data URI 由服务端直传;数量上限由 DrawService 按 max_ref_images 校验)
        // Image-to-image: Kolors takes a single image param (server-relayed data URI; count validated by DrawService per max_ref_images)
        List<String> refImageUuids = draw.getRefImageUuids();
        if (!refImageUuids.isEmpty()) {
            builder.refImageDataUrl(toImageDataUrl(refImageUuids.get(0)));
        }
        if (null != proxyAddress && platform.getIsProxyEnable()) {
            HttpClient.Builder httpClientBuilder = HttpClient.newBuilder().proxy(ProxySelector.of(proxyAddress));
            builder.httpClientBuilder(JdkHttpClient.builder().httpClientBuilder(httpClientBuilder));
        }
        return builder.build();
    }

    /**
     * 参考图文件转 base64 data URI(硅基流动要求 data URI 前缀格式):本地存储读盘、OSS 从可访问 URL 下载
     * <p>
     * Reference image to base64 data URI (SiliconFlow requires the data URI prefix format).
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
        return LLMException.builder().message(error.toString()).build();
    }
}

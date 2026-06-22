package com.moyz.adi.common.languagemodel.wanx;

import com.alibaba.dashscope.aigc.imagegeneration.ImageGeneration;
import com.alibaba.dashscope.aigc.imagegeneration.ImageGenerationMessage;
import com.alibaba.dashscope.aigc.imagegeneration.ImageGenerationParam;
import com.alibaba.dashscope.aigc.imagegeneration.ImageGenerationResult;
import com.alibaba.dashscope.exception.NoApiKeyException;
import com.alibaba.dashscope.exception.UploadFileException;
import com.moyz.adi.common.util.ImageUtil;
import dev.langchain4j.data.image.Image;
import dev.langchain4j.internal.Utils;
import dev.langchain4j.model.image.ImageModel;
import dev.langchain4j.model.output.Response;
import lombok.extern.slf4j.Slf4j;

import java.util.Collections;
import java.util.List;

/**
 * wan2.7-image 文生图模型封装,基于 DashScope 新一代 image-generation 接口(multimodal messages 结构)。
 * <p>
 * 与旧 {@link AdiWanxImageModel}(走 image-synthesis 端点)不同,本类调用同步 image-generation 端点,
 * 且新接口已移除 negative_prompt,故不暴露该参数。
 */
@Slf4j
public class AdiImageGenerationModel implements ImageModel {

    private final String apiKey;
    private final String modelName;
    private final String size;
    private final Integer seed;
    private final ImageGeneration imageGeneration;

    public AdiImageGenerationModel(String apiKey, String modelName, String size, Integer seed, String baseUrl) {
        if (Utils.isNullOrBlank(apiKey)) {
            throw new IllegalArgumentException(
                    "DashScope api key must be defined. It can be generated here: https://dashscope.console.aliyun.com/apiKey");
        }
        this.apiKey = apiKey;
        this.modelName = Utils.isNullOrBlank(modelName) ? "wan2.7-image" : modelName;
        this.size = size;
        this.seed = seed;
        // 支持自定义 endpoint(私有部署/代理),与旧 AdiWanxImageModel 行为一致
        this.imageGeneration = Utils.isNullOrBlank(baseUrl)
                ? new ImageGeneration()
                : new ImageGeneration("http", baseUrl);
    }

    @Override
    public Response<Image> generate(String prompt) {
        return Response.from(generateImages(prompt, 1).get(0));
    }

    @Override
    public Response<List<Image>> generate(String prompt, int n) {
        return Response.from(generateImages(prompt, n));
    }

    /**
     * 图像编辑(图生图)本次未实现 —— 用户选择"只做文生图平替"。
     * <p>
     * Image editing is not implemented in this iteration (text-to-image parity only).
     */
    @Override
    public Response<Image> edit(Image image, String prompt) {
        throw new UnsupportedOperationException(
                "Image editing is not supported yet for wan2.7-image (text-to-image parity only)");
    }

    private List<Image> generateImages(String prompt, int n) {
        ImageGenerationMessage message = ImageGenerationMessage.builder()
                .role("user")
                .content(Collections.singletonList(Collections.singletonMap("text", prompt)))
                .build();

        var builder = ImageGenerationParam.builder()
                .apiKey(apiKey)
                .model(modelName)
                .messages(Collections.singletonList(message))
                .n(n);

        if (Utils.isNotNullOrBlank(size)) {
            builder.size(size);
        }
        if (seed != null) {
            builder.seed(seed);
        }

        try {
            ImageGenerationResult result = imageGeneration.call(builder.build());
            List<Image> images = ImageUtil.imagesFrom(result);
            if (images.isEmpty()) {
                log.error("wan2.7-image returned no image, output:{}", result.getOutput());
                throw new IllegalStateException("wan2.7-image returned no image");
            }
            return images;
        } catch (NoApiKeyException e) {
            // apiKey 缺失转为参数异常,与旧 AdiWanxImageModel 一致
            throw new IllegalArgumentException(e);
        } catch (UploadFileException e) {
            // wan2.7 纯文生图不传图,理论上不触发
            throw new RuntimeException(e);
        }
        // ApiException(RuntimeException) 不在此捕获,自动传播给 DashScopeWanxService.parseError 解析
    }

    public static AdiImageGenerationModelBuilder builder() {
        return new AdiImageGenerationModelBuilder();
    }

    public static class AdiImageGenerationModelBuilder {
        private String apiKey;
        private String modelName;
        private String size;
        private Integer seed;
        private String baseUrl;

        public AdiImageGenerationModelBuilder apiKey(String apiKey) {
            this.apiKey = apiKey;
            return this;
        }

        public AdiImageGenerationModelBuilder modelName(String modelName) {
            this.modelName = modelName;
            return this;
        }

        public AdiImageGenerationModelBuilder size(String size) {
            this.size = size;
            return this;
        }

        public AdiImageGenerationModelBuilder seed(Integer seed) {
            this.seed = seed;
            return this;
        }

        public AdiImageGenerationModelBuilder baseUrl(String baseUrl) {
            this.baseUrl = baseUrl;
            return this;
        }

        public AdiImageGenerationModel build() {
            return new AdiImageGenerationModel(apiKey, modelName, size, seed, baseUrl);
        }
    }
}

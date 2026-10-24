package com.moyz.adi.common.languagemodel;

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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * DashScope 图片模型封装(wan2.7-image / qwen-image-3.0),基于新一代 image-generation 接口(multimodal messages 结构)。
 * <p>
 * 文生图与图生图同模型同端点:不传参考图为文生图;构造时传入 refImageDataUrls(base64 data URI)
 * 即为图生图,参考图由服务端直传,不依赖文件公网可访问。新接口已移除 negative_prompt,故不暴露该参数。
 */
@Slf4j
public class AdiImageGenerationModel implements ImageModel {

    private final String apiKey;
    private final String modelName;
    private final String size;
    private final Integer seed;
    /**
     * 图生图参考图(base64 data URI),null 或空表示文生图
     */
    private final List<String> refImageDataUrls;
    private final ImageGeneration imageGeneration;

    public AdiImageGenerationModel(String apiKey, String modelName, String size, Integer seed,
                                   List<String> refImageDataUrls, String baseUrl) {
        if (Utils.isNullOrBlank(apiKey)) {
            throw new IllegalArgumentException(
                    "DashScope api key must be defined. It can be generated here: https://dashscope.console.aliyun.com/apiKey");
        }
        this.apiKey = apiKey;
        this.modelName = Utils.isNullOrBlank(modelName) ? "wan2.7-image" : modelName;
        this.size = size;
        this.seed = seed;
        this.refImageDataUrls = refImageDataUrls;
        // 支持自定义 endpoint(私有部署/代理),与旧实现行为一致
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
     * 图生图经构造参数 refImageDataUrls 传入,不走本方法。
     * <p>
     * Image-to-image is carried by the constructor arg refImageDataUrls, not by this method.
     */
    @Override
    public Response<Image> edit(Image image, String prompt) {
        throw new UnsupportedOperationException(
                "Use builder refImageDataUrls for image-to-image generation");
    }

    private List<Image> generateImages(String prompt, int n) {
        // 参考图项在前、提示词项在后,与 DashScope 多模态消息约定一致
        List<Map<String, Object>> contentItems = new ArrayList<>();
        if (null != refImageDataUrls) {
            for (String dataUrl : refImageDataUrls) {
                contentItems.add(Collections.singletonMap("image", (Object) dataUrl));
            }
        }
        contentItems.add(Collections.singletonMap("text", (Object) prompt));
        ImageGenerationMessage message = ImageGenerationMessage.builder()
                .role("user")
                .content(contentItems)
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
                log.error("{} returned no image, output:{}", modelName, result.getOutput());
                throw new IllegalStateException(modelName + " returned no image");
            }
            return images;
        } catch (NoApiKeyException e) {
            // apiKey 缺失转为参数异常,与旧实现一致
            throw new IllegalArgumentException(e);
        } catch (UploadFileException e) {
            // 参考图走 content 内 base64 data URI,不经 SDK 文件上传通道,理论上不触发
            throw new RuntimeException(e);
        }
        // ApiException(RuntimeException) 不在此捕获,自动传播给 DashScopeImageService.parseError 解析
    }

    public static AdiImageGenerationModelBuilder builder() {
        return new AdiImageGenerationModelBuilder();
    }

    public static class AdiImageGenerationModelBuilder {
        private String apiKey;
        private String modelName;
        private String size;
        private Integer seed;
        private List<String> refImageDataUrls;
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

        public AdiImageGenerationModelBuilder refImageDataUrls(List<String> refImageDataUrls) {
            this.refImageDataUrls = refImageDataUrls;
            return this;
        }

        public AdiImageGenerationModelBuilder baseUrl(String baseUrl) {
            this.baseUrl = baseUrl;
            return this;
        }

        public AdiImageGenerationModel build() {
            return new AdiImageGenerationModel(apiKey, modelName, size, seed, refImageDataUrls, baseUrl);
        }
    }
}

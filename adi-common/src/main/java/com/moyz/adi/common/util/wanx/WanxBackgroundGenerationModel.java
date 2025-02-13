package com.moyz.adi.common.util.wanx;

import com.alibaba.dashscope.aigc.imagesynthesis.ImageSynthesisOutput;
import com.alibaba.dashscope.aigc.imagesynthesis.ImageSynthesisParam;
import com.alibaba.dashscope.aigc.imagesynthesis.ImageSynthesisResult;
import com.alibaba.dashscope.exception.NoApiKeyException;
import dev.langchain4j.data.image.Image;
import dev.langchain4j.internal.Utils;
import dev.langchain4j.model.dashscope.WanxModelName;
import dev.langchain4j.model.image.ImageModel;
import dev.langchain4j.model.output.Response;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import java.util.List;
import java.util.function.Consumer;

import static com.moyz.adi.common.cosntant.AdiConstant.W_FAILED;
import static com.moyz.adi.common.util.ImageUtil.imageUrl;
import static com.moyz.adi.common.util.ImageUtil.imagesFrom;
import static dev.langchain4j.internal.ValidationUtils.ensureNotNull;

/**
 * Represents a Wanx models to generate artistic images.
 * More details are available <a href="https://help.aliyun.com/zh/dashscope/developer-reference/api-details-9">here</a>.
 */
@Slf4j
public class WanxBackgroundGenerationModel implements ImageModel {

    private final String apiKey;
    private final String modelName;
    private final String size;
    private final BackgroundGeneration backgroundGeneration;
    private final String baseImageUrl;
    private final String refImageUrl;
    private final String refPrompt;

    private Consumer<ImageSynthesisParam.ImageSynthesisParamBuilder<?, ?>> imageSynthesisParamCustomizer = p -> {
    };

    public WanxBackgroundGenerationModel(
            String baseUrl,
            String apiKey,
            String modelName,
            String size,
            String baseImageUrl,
            String refImageUrl,
            String refPrompt) {
        if (Utils.isNullOrBlank(apiKey)) {
            throw new IllegalArgumentException(
                    "DashScope api key must be defined. It can be generated here: https://dashscope.console.aliyun.com/apiKey");
        }
        this.modelName = Utils.isNullOrBlank(modelName) ? WanxModelName.WANX_V1 : modelName;
        this.apiKey = apiKey;
        this.baseImageUrl = baseImageUrl;
        this.refImageUrl = refImageUrl;
        this.refPrompt = refPrompt;
        this.size = size;
        this.backgroundGeneration =
                Utils.isNullOrBlank(baseUrl) ? new BackgroundGeneration() : new BackgroundGeneration("background-generation", baseUrl);
    }

    @Override
    public Response<Image> generate(String prompt) {
        ImageSynthesisParam.ImageSynthesisParamBuilder<?, ?> builder =
                requestBuilder(prompt).n(1);

        try {
            imageSynthesisParamCustomizer.accept(builder);
            ImageSynthesisResult result = backgroundGeneration.call(builder.build());
            return Response.from(imagesFrom(result).get(0));
        } catch (NoApiKeyException e) {
            throw new IllegalArgumentException(e);
        }
    }

    @Override
    public Response<List<Image>> generate(String prompt, int n) {
        ImageSynthesisParam.ImageSynthesisParamBuilder<?, ?> builder =
                requestBuilder(prompt).n(n);
        builder.extraInput("base_image_url", baseImageUrl);
        if (StringUtils.isNotBlank(refImageUrl)) {
            builder.extraInput("ref_image_url", refImageUrl);
        }
        if (StringUtils.isNotBlank(refPrompt)) {
            builder.extraInput("ref_prompt", refPrompt);
        }
        try {
            imageSynthesisParamCustomizer.accept(builder);
            ImageSynthesisResult result = backgroundGeneration.call(builder.build());
            if (result.getOutput().getTaskStatus().equalsIgnoreCase(W_FAILED)) {
                log.error("Wanx failed to generate images: {}", result.getOutput().getMessage());
                throw new InternalError(result.getOutput().getCode());
            }
            return Response.from(imagesFrom(result));
        } catch (NoApiKeyException e) {
            throw new IllegalArgumentException(e);
        }
    }

    @Override
    public Response<Image> edit(Image image, String prompt) {
        String imageUrl = imageUrl(image, modelName, apiKey);

        ImageSynthesisParam.ImageSynthesisParamBuilder<?, ?> builder =
                requestBuilder(prompt).refImage(imageUrl).n(1);

        if (imageUrl.startsWith("oss://")) {
            builder.header("X-DashScope-OssResourceResolve", "enable");
        }

        try {
            imageSynthesisParamCustomizer.accept(builder);
            ImageSynthesisResult result = backgroundGeneration.call(builder.build());
            List<Image> images = imagesFrom(result);
            if (images.isEmpty()) {
                ImageSynthesisOutput output = result.getOutput();
                String errorMessage =
                        String.format("[%s] %s: %s", output.getTaskStatus(), output.getCode(), output.getMessage());
                throw new IllegalStateException(errorMessage);
            }
            return Response.from(images.get(0));
        } catch (NoApiKeyException e) {
            throw new IllegalArgumentException(e);
        }
    }

    public void setImageSynthesisParamCustomizer(
            Consumer<ImageSynthesisParam.ImageSynthesisParamBuilder<?, ?>> imageSynthesisParamCustomizer) {
        this.imageSynthesisParamCustomizer =
                ensureNotNull(imageSynthesisParamCustomizer, "imageSynthesisParamCustomizer");
    }

    private ImageSynthesisParam.ImageSynthesisParamBuilder<?, ?> requestBuilder(String prompt) {
        ImageSynthesisParam.ImageSynthesisParamBuilder<?, ?> builder =
                ImageSynthesisParam.builder()
                        .apiKey(apiKey)
                        .model(modelName)
                        .prompt(prompt);

        if (size != null) {
            builder.size(size);
        }

        return builder;
    }

    public static WanxImageModelBuilder builder() {
        return new WanxImageModelBuilder();
    }

    public static class WanxImageModelBuilder {

        private String baseUrl;
        private String apiKey;
        private String modelName;
        private String size;
        private String baseImageUrl;
        private String refImageUrl;
        private String refPrompt;

        public WanxImageModelBuilder() {
            // This is public, so it can be extended
            // By default with Lombok it becomes package private
        }

        public WanxImageModelBuilder baseUrl(String baseUrl) {
            this.baseUrl = baseUrl;
            return this;
        }

        public WanxImageModelBuilder apiKey(String apiKey) {
            this.apiKey = apiKey;
            return this;
        }

        public WanxImageModelBuilder modelName(String modelName) {
            this.modelName = modelName;
            return this;
        }


        public WanxImageModelBuilder size(String size) {
            this.size = size;
            return this;
        }

        public WanxImageModelBuilder baseImageUrl(String baseImageUrl) {
            this.baseImageUrl = baseImageUrl;
            return this;
        }

        public WanxImageModelBuilder refImageUrl(String refImageUrl) {
            this.refImageUrl = refImageUrl;
            return this;
        }

        public WanxImageModelBuilder refPrompt(String refPrompt) {
            this.refPrompt = refPrompt;
            return this;
        }

        public WanxBackgroundGenerationModel build() {
            return new WanxBackgroundGenerationModel(baseUrl, apiKey, modelName, size, baseImageUrl, refImageUrl, refPrompt);
        }
    }

}
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
public class AdiWanxImageModel implements ImageModel {

    private final String task;
    private final String function;
    private final String apiKey;
    private final String modelName;
    private final Integer seed;
    //Default is '1024*1024'.
    private final String size;
    private final AdiImageSynthesis imageSynthesis;
    private final String negativePrompt;
    private Consumer<ImageSynthesisParam.ImageSynthesisParamBuilder<?, ?>> imageSynthesisParamCustomizer = p -> {
    };

    public AdiWanxImageModel(
            String baseUrl,
            String apiKey,
            String modelName,
            String task,
            String function,
            Integer seed,
            String size,
            String negativePrompt) {
        if (Utils.isNullOrBlank(apiKey)) {
            throw new IllegalArgumentException(
                    "DashScope api key must be defined. It can be generated here: https://dashscope.console.aliyun.com/apiKey");
        }
        this.modelName = Utils.isNullOrBlank(modelName) ? WanxModelName.WANX_V1 : modelName;
        this.apiKey = apiKey;
        this.seed = seed;
        this.size = size;
        this.negativePrompt = negativePrompt;
        this.task = Utils.isNullOrBlank(task) ? "text2image" : task;
        this.function = Utils.isNullOrBlank(function) ? "image-synthesis" : function;
        this.imageSynthesis =
                Utils.isNullOrBlank(baseUrl) ? new AdiImageSynthesis(this.task, this.function) : new AdiImageSynthesis(this.task, this.function, baseUrl);
    }

    @Override
    public Response<Image> generate(String prompt) {
        ImageSynthesisParam.ImageSynthesisParamBuilder<?, ?> builder =
                requestBuilder(prompt).n(1);

        try {
            imageSynthesisParamCustomizer.accept(builder);
            ImageSynthesisResult result = imageSynthesis.call(builder.build());
            return Response.from(imagesFrom(result).get(0));
        } catch (NoApiKeyException e) {
            throw new IllegalArgumentException(e);
        }
    }

    @Override
    public Response<List<Image>> generate(String prompt, int n) {
        ImageSynthesisParam.ImageSynthesisParamBuilder<?, ?> builder =
                requestBuilder(prompt).n(n);

        try {
            imageSynthesisParamCustomizer.accept(builder);
            ImageSynthesisResult result = imageSynthesis.call(builder.build());
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
            ImageSynthesisResult result = imageSynthesis.call(builder.build());
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
                        .negativePrompt(negativePrompt)
                        .prompt(prompt);

        if (seed != null) {
            builder.seed(seed);
        }

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
        private String task;
        private String function;
        private Integer seed;
        private String size;
        private String negativePrompt;

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

        public WanxImageModelBuilder seed(Integer seed) {
            this.seed = seed;
            return this;
        }

        public WanxImageModelBuilder size(String size) {
            this.size = size;
            return this;
        }

        public WanxImageModelBuilder task(String task) {
            this.task = task;
            return this;
        }

        public WanxImageModelBuilder function(String function) {
            this.function = function;
            return this;
        }

        public WanxImageModelBuilder negativePrompt(String negativePrompt) {
            this.negativePrompt = negativePrompt;
            return this;
        }

        public AdiWanxImageModel build() {
            return new AdiWanxImageModel(baseUrl, apiKey, modelName, task, function, seed, size, negativePrompt);
        }
    }

}
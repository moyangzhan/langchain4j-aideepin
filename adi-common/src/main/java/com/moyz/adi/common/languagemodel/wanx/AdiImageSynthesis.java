package com.moyz.adi.common.languagemodel.wanx;

import com.alibaba.dashscope.aigc.imagesynthesis.ImageSynthesisListResult;
import com.alibaba.dashscope.aigc.imagesynthesis.ImageSynthesisParam;
import com.alibaba.dashscope.aigc.imagesynthesis.ImageSynthesisResult;
import com.alibaba.dashscope.aigc.imagesynthesis.SketchImageSynthesisParam;
import com.alibaba.dashscope.api.AsynchronousApi;
import com.alibaba.dashscope.base.HalfDuplexServiceParam;
import com.alibaba.dashscope.common.DashScopeResult;
import com.alibaba.dashscope.exception.ApiException;
import com.alibaba.dashscope.exception.NoApiKeyException;
import com.alibaba.dashscope.protocol.ApiServiceOption;
import com.alibaba.dashscope.protocol.HttpMethod;
import com.alibaba.dashscope.protocol.Protocol;
import com.alibaba.dashscope.protocol.StreamingMode;
import com.alibaba.dashscope.task.AsyncTaskListParam;
import com.aliyun.core.utils.StringUtils;

public final class AdiImageSynthesis {
    private final AsynchronousApi<HalfDuplexServiceParam> asyncApi;
    private final ApiServiceOption createServiceOptions;
    private final String baseUrl;

    public static class Models {
        public static final String WANX_V1 = "wanx-v1";
        public static final String WANX_V2 = "wanx-v2";
        public static final String WANX_SKETCH_TO_IMAGE_V1 = "wanx-sketch-to-image-v1";
        public static final String WANX_BACKGROUND_GENERATION_V2 = "wanx-background-generation-v2";
    }

    /**
     * Default test2image
     */
    public AdiImageSynthesis() {
        // only support http
        asyncApi = new AsynchronousApi<HalfDuplexServiceParam>();
        createServiceOptions =
                ApiServiceOption.builder()
                        .protocol(Protocol.HTTP)
                        .httpMethod(HttpMethod.POST)
                        .streamingMode(StreamingMode.NONE)
                        .taskGroup("aigc")
                        .task("text2image")
                        .function("image-synthesis")
                        .isAsyncTask(true)
                        .build();
        this.baseUrl = null;
    }

    /**
     * The task of the image synthesis image2image or test2image.
     *
     * @param task The task of image synthesis(image2image|text2image).
     */
    public AdiImageSynthesis(String task, String function) {
        // only support http
        asyncApi = new AsynchronousApi<HalfDuplexServiceParam>();
        createServiceOptions =
                ApiServiceOption.builder()
                        .protocol(Protocol.HTTP)
                        .httpMethod(HttpMethod.POST)
                        .streamingMode(StreamingMode.NONE)
                        .taskGroup("aigc")
                        .task(task)
                        .function(StringUtils.isBlank(function) ? "image-synthesis" : function)
                        .isAsyncTask(true)
                        .build();
        this.baseUrl = null;
    }

    /**
     * Create with task and custom baseUrl
     *
     * @param task    The task of image synthesis(image2image|text2image).
     * @param baseUrl The service base url.
     */
    public AdiImageSynthesis(String task, String function, String baseUrl) {
        // only support http
        asyncApi = new AsynchronousApi<HalfDuplexServiceParam>();
        createServiceOptions =
                ApiServiceOption.builder()
                        .protocol(Protocol.HTTP)
                        .httpMethod(HttpMethod.POST)
                        .baseHttpUrl(baseUrl)
                        .streamingMode(StreamingMode.NONE)
                        .taskGroup("aigc")
                        .task(task)
                        .function(StringUtils.isBlank(function) ? "image-synthesis" : function)
                        .isAsyncTask(true)
                        .build();
        this.baseUrl = baseUrl;
    }

    public ImageSynthesisResult asyncCall(ImageSynthesisParam param)
            throws ApiException, NoApiKeyException {
        return ImageSynthesisResult.fromDashScopeResult(
                asyncApi.asyncCall(param, createServiceOptions));
    }

    /**
     * Call the server to get the result.
     *
     * @param param The input param of class `ImageSynthesisParam`.
     * @return The image synthesis result.
     * @throws NoApiKeyException Can not find api key.
     * @throws ApiException      The request failed, possibly due to a network or data error.
     */
    public ImageSynthesisResult call(ImageSynthesisParam param)
            throws ApiException, NoApiKeyException {
        return ImageSynthesisResult.fromDashScopeResult(asyncApi.call(param, createServiceOptions));
    }

    /**
     * @param param The input param of class `SketchImageSynthesisParam`
     * @return The image synthesis result `ImageSynthesisResult`
     * @throws ApiException      The dashscope exception.
     * @throws NoApiKeyException No api key provide.
     */
    public ImageSynthesisResult asyncCall(SketchImageSynthesisParam param)
            throws ApiException, NoApiKeyException {
        return ImageSynthesisResult.fromDashScopeResult(
                asyncApi.asyncCall(param, createServiceOptions));
    }

    /**
     * Call the server to get the result.
     *
     * @param param The input param of class `SketchImageSynthesisParam`.
     * @return The image synthesis result.
     * @throws NoApiKeyException Can not find api key.
     * @throws ApiException      The request failed, possibly due to a network or data error.
     */
    public ImageSynthesisResult call(SketchImageSynthesisParam param)
            throws ApiException, NoApiKeyException {
        return ImageSynthesisResult.fromDashScopeResult(asyncApi.call(param, createServiceOptions));
    }

    public ImageSynthesisListResult list(AsyncTaskListParam param)
            throws ApiException, NoApiKeyException {
        return ImageSynthesisListResult.fromDashScopeResult(asyncApi.list(param, baseUrl));
    }

    public ImageSynthesisListResult list(
            String startTime,
            String endTime,
            String modelName,
            String apiKeyId,
            String region,
            String status,
            Integer pageNo,
            Integer pageSize)
            throws ApiException, NoApiKeyException {
        return ImageSynthesisListResult.fromDashScopeResult(
                asyncApi.list(
                        startTime, endTime, modelName, apiKeyId, region, status, pageNo, pageSize, baseUrl));
    }

    public ImageSynthesisResult fetch(String taskId, String apiKey)
            throws ApiException, NoApiKeyException {
        return ImageSynthesisResult.fromDashScopeResult(asyncApi.fetch(taskId, apiKey, baseUrl));
    }

    public ImageSynthesisResult fetch(ImageSynthesisResult taskInfo, String apiKey)
            throws ApiException, NoApiKeyException {

        return ImageSynthesisResult.fromDashScopeResult(
                asyncApi.fetch(taskInfo.getOutput().getTaskId(), apiKey, baseUrl));
    }

    public ImageSynthesisResult cancel(String taskId, String apiKey)
            throws ApiException, NoApiKeyException {
        return ImageSynthesisResult.fromDashScopeResult(asyncApi.cancel(taskId, apiKey, baseUrl));
    }

    public ImageSynthesisResult cancel(ImageSynthesisResult taskInfo, String apiKey)
            throws ApiException, NoApiKeyException {
        DashScopeResult res = asyncApi.cancel(taskInfo.getOutput().getTaskId(), apiKey, baseUrl);
        return ImageSynthesisResult.fromDashScopeResult(res);
    }

    public ImageSynthesisResult wait(String taskId, String apiKey)
            throws ApiException, NoApiKeyException {
        return ImageSynthesisResult.fromDashScopeResult(asyncApi.wait(taskId, apiKey, baseUrl));
    }

    public ImageSynthesisResult wait(ImageSynthesisResult taskInfo, String apiKey)
            throws ApiException, NoApiKeyException {
        return ImageSynthesisResult.fromDashScopeResult(
                asyncApi.wait(taskInfo.getOutput().getTaskId(), apiKey, baseUrl));
    }
}
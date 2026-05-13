package com.moyz.adi.common.languagemodel;

import com.alibaba.dashscope.utils.JsonUtils;
import com.moyz.adi.common.languagemodel.data.SiliconflowImageGenerateReq;
import com.moyz.adi.common.languagemodel.data.SiliconflowImageGenerateResp;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.util.JsonUtil;
import dev.langchain4j.data.image.Image;
import dev.langchain4j.http.client.HttpClientBuilder;
import dev.langchain4j.http.client.HttpRequest;
import dev.langchain4j.http.client.SuccessfulHttpResponse;
import dev.langchain4j.http.client.jdk.JdkHttpClient;
import dev.langchain4j.model.image.ImageModel;
import dev.langchain4j.model.output.Response;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static dev.langchain4j.http.client.HttpMethod.POST;

/**
 * 硅基流动-图像生成模型
 */
@Builder
@AllArgsConstructor
@Slf4j
public class SiliconflowImageModel implements ImageModel {

    private final String baseUrl;
    private final String apiKey;

    /**
     * Available options: Qwen/Qwen-Image-Edit-2509, Qwen/Qwen-Image-Edit, Qwen/Qwen-Image, Kwai-Kolors/Kolors
     */
    private final String modelName;


    /**
     * For Kolor model:
     * "1024x1024" (1:1)
     * "960x1280" (3:4)
     * "768x1024" (3:4)
     * "720x1440" (1:2)
     * "720x1280" (9:16)
     * For Qwen-Image model:
     * "1328x1328" (1:1)
     * "1664x928" (16:9)
     * "928x1664" (9:16)
     * "1472x1140" (4:3)
     * "1140x1472" (3:4)
     * "1584x1056" (3:2)
     * "1056x1584" (2:3)
     */
    private final String imageSize;

    /**
     * negative prompt
     */
    private final String negativePrompt;

    /**
     * number of output images. Only applicable to Kwai-Kolors/Kolors.
     * <p>
     * Required range: 1 <= x <= 4
     */
    private final Integer batchSize;

    /**
     * Required range: 0 <= x <= 9999999999
     */
    private final Long seed;

    private HttpClientBuilder httpClientBuilder;

    @Override
    public Response<Image> generate(String prompt) {
        List<Image> list = internalGenerate(prompt, 1);
        if (list.isEmpty()) {
            return Response.from(Image.builder().url("").build());
        } else {
            return Response.from(list.get(0));
        }
    }

    @Override
    public Response<List<Image>> generate(String prompt, int n) {
        List<Image> result = internalGenerate(prompt, n);
        return Response.from(result);
    }

    @Override
    public Response<Image> edit(Image image, String prompt) {
        return ImageModel.super.edit(image, prompt);
    }

    @Override
    public Response<Image> edit(Image image, Image mask, String prompt) {
        return ImageModel.super.edit(image, mask, prompt);
    }

    private List<Image> internalGenerate(String prompt, int n) {
        SiliconflowImageGenerateReq request = SiliconflowImageGenerateReq.builder()
                .prompt(prompt)
                .model(modelName)
                .batchSize(n)
                .imageSize(imageSize)
                .negativePrompt(negativePrompt)
                .seed(seed)
                .build();
        Map<String, String> defaultHeaders = new HashMap<>();
        defaultHeaders.put("Authorization", "Bearer " + apiKey);
        HttpRequest httpRequest = HttpRequest.builder()
                .method(POST)
                .url(baseUrl, "images/generations")
                .addHeader("Content-Type", "application/json")
                .addHeaders(defaultHeaders)
                .body(JsonUtil.toJson(request))
                .build();
        SuccessfulHttpResponse rawHttpResponse;
        if (null != httpClientBuilder) {
            rawHttpResponse = httpClientBuilder.build().execute(httpRequest);
        } else {
            rawHttpResponse = JdkHttpClient.builder()
                    .build()
                    .execute(httpRequest);
        }
        int statusCode = rawHttpResponse.statusCode();
        if (statusCode != 200) {
            log.error("Siliconflow draw fail, statusCode: {}, body: {}", statusCode, rawHttpResponse.body());
            if (statusCode == 400 || statusCode == 429 || statusCode == 503) {
                Map<String, Object> errMap = JsonUtil.toMap(rawHttpResponse.body());
                throw new BaseException(ErrorEnum.C_DRAW_FAIL, errMap.get("message").toString());
            }
            throw new BaseException(ErrorEnum.C_DRAW_FAIL, rawHttpResponse.body());
        }
        SiliconflowImageGenerateResp parsedResponse = JsonUtils.fromJson(rawHttpResponse.body(), SiliconflowImageGenerateResp.class);
        if (null == parsedResponse) {
            throw new BaseException(ErrorEnum.C_DRAW_FAIL, "Siliconflow response is null");
        }
        List<Image> result = new ArrayList<>();
        for (SiliconflowImageGenerateResp.Image image : parsedResponse.getImages()) {
            Image langchainImage = Image.builder().url(image.getUrl()).build();
            result.add(langchainImage);
        }
        return result;
    }
}

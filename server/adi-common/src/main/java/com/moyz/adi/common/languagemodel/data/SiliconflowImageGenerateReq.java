package com.moyz.adi.common.languagemodel.data;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class SiliconflowImageGenerateReq {

    /**
     * Available options: Qwen/Qwen-Image-Edit-2509, Qwen/Qwen-Image-Edit, Qwen/Qwen-Image, Kwai-Kolors/Kolors
     */
    private String model;

    private String prompt;

    @JsonProperty("negative_prompt")
    private String negativePrompt;

    @JsonProperty("image_size")
    private String imageSize;

    /**
     * number of output images. Only applicable to Kwai-Kolors/Kolors.
     * <p>
     * Required range: 1 <= x <= 4
     */
    @JsonProperty("batch_size")
    private Integer batchSize;

    /**
     * Required range: 0 <= x <= 9999999999
     */
    private Long seed;

    /**
     * 图生图输入图片(data URI 或图片 URL),Kolors 支持;null 为纯文生图。
     * (image2/image3 仅 Qwen/Qwen-Image-Edit-2509 支持多图,暂未使用)
     * <p>
     * Image-to-image input (data URI or image URL), supported by Kolors; null for text-to-image.
     */
    private String image;
}

package com.moyz.adi.common.languagemodel.data;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ImageModelBuilderProperties {

    /**
     * gpt-image-2: multiples of 16px, max edge 3840px, ratio <= 3:1.
     * Popular sizes: 1024x1024, 1024x1536, 1536x1024.
     */
    private String size;

    /**
     * OpenAI image models only.
     * gpt-image-2 quality: low, medium, high.
     */
    private String quality;

    //通义万相
    private String modelName;

}

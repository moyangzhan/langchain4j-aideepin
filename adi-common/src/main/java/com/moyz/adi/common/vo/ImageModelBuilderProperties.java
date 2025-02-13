package com.moyz.adi.common.vo;

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
     * When using DALL·E 2, images can have a size of 256X256, 768x768 or 1024x1024 pixels.
     * When using DALL·E 3, images can have a size of 1024x1024, 1024x1792 or 1792x1024 pixels.
     * 通义万相：默认1024*1024，可任意组合
     */
    private String size;

    /**
     * Dalle3 only.
     * By default, images are generated at standard quality, but when using DALL·E 3 you can set quality: "hd" for enhanced detail. Square, standard quality images are the fastest to generate.
     */
    private String quality;

    //通义万相
    private String modelName;

}

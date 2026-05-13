package com.moyz.adi.common.languagemodel.data;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Map;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class SiliconflowImageGenerateResp {
    private List<Image> images;
    private Map<String, Object> timings;
    private Integer seed;

    @Data
    public static class Image {
        private String url;
    }
}

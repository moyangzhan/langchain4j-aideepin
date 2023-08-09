package com.moyz.adi.common.dto;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;

@Data
public class AiImageDto {
    private Long id;
    private String uuid;
    private String prompt;

    private String originalImageUrl;

    private String maskImageUrl;

    private Integer interactingMethod;

    @JsonIgnore
    private String generatedImages;
    /**
     * http url
     */
    private List<String> imageUrlList;
    private Integer processStatus;
    private LocalDateTime createTime;
}

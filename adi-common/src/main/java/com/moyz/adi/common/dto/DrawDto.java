package com.moyz.adi.common.dto;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;

@Data
public class DrawDto {
    private Long id;
    private String uuid;
    private String prompt;
    private String aiModelName;
    private String originalImageUuid;

    private String maskImageUuid;

    private Integer interactingMethod;

    private Boolean isPublic;

    @JsonIgnore
    private String generatedImages;
    private List<String> imageUuids;
    private Integer processStatus;
    private LocalDateTime createTime;
}

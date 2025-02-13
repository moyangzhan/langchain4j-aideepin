package com.moyz.adi.common.dto;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.databind.JsonNode;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;

@Data
public class DrawDto {
    private Long id;
    private String uuid;
    private String prompt;
    private String aiModelName;
    private Integer interactingMethod;
    private Boolean isPublic;
    private Integer starCount;
    private Integer processStatus;
    private String processStatusRemark;
    @JsonIgnore
    private String generatedImages;
    @JsonIgnore
    private Long userId;
    private LocalDateTime createTime;
    private JsonNode dynamicParams;

    //非 Draw 字段
    private Boolean isStar;
    private String originalImageUuid;
    private String originalImageUrl;
    private String maskImageUuid;
    private String maskImageUrl;
    private String userUuid;
    private String userName;
    private List<String> imageUuids;
    private List<String> imageUrls;
}

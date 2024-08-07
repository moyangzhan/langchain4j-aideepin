package com.moyz.adi.common.dto;

import lombok.Data;

@Data
public class CreateImageDto {
    private String prompt;
    private String size;
    private String quality;
    private int number;
    private int interactingMethod;
    private String originalImage;
    private String maskImage;
    private String modelName;
}

package com.moyz.adi.common.dto;

import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Data
public class GenerateImageReq {
    @NotBlank
    private String prompt;
    @NotBlank
    private String size;
    private String quality;
    @Min(1)
    @Max(10)
    private int number;
    private String modelName;
}

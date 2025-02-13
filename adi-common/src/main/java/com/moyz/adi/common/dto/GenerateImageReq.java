package com.moyz.adi.common.dto;

import com.fasterxml.jackson.databind.JsonNode;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Data
public class GenerateImageReq {
    @NotBlank
    private String prompt;
    private String negativePrompt;
    private String size;
    private String quality;
    @Min(1)
    @Max(10)
    private int number;
    private String modelName;
    private int seed;
    private JsonNode dynamicParams;
    private int interactingMethod;
}

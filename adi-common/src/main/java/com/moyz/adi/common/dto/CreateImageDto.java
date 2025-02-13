package com.moyz.adi.common.dto;

import com.fasterxml.jackson.databind.JsonNode;
import lombok.Data;

@Data
public class CreateImageDto {
    private String prompt;
    private String negativePrompt;
    private String size;
    private String quality;
    private int number;
    private int interactingMethod;
    private int seed;
    private String originalImage;
    private String maskImage;
    private String modelName;
    private JsonNode dynamicParams;
}

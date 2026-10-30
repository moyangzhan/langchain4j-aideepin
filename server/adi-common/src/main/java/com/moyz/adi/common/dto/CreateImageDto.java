package com.moyz.adi.common.dto;

import tools.jackson.databind.JsonNode;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
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

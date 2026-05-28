package com.moyz.adi.common.dto.extapi;

import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ExtApiDrawReq {

    @NotBlank(message = "prompt is required")
    private String prompt;

    private String negativePrompt;

    private String size;

    private String quality;

    @Min(1)
    @Builder.Default
    private int number = 1;

    private String model;

    private int seed;
}

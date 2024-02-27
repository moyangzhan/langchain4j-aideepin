package com.moyz.adi.common.dto;

import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;
import org.hibernate.validator.constraints.Length;

@Data
public class VariationImageReq {
    @Length(min = 32, max = 32)
    private String originalImage;
    @NotBlank
    private String size;
    @Min(1)
    @Max(10)
    private int number;

    private String modelName;
}

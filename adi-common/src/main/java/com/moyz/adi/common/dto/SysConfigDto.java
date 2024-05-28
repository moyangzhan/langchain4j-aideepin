package com.moyz.adi.common.dto;

import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;
import org.springframework.validation.annotation.Validated;

@Data
@Validated
public class SysConfigDto {

    @Min(1)
    private Long id;
    @NotBlank
    private String name;
    @NotBlank
    private String value;
}

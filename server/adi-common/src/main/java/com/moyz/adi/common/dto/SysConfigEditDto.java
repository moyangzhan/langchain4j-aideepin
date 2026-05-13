package com.moyz.adi.common.dto;

import jakarta.validation.constraints.NotBlank;
import lombok.Data;
import org.springframework.validation.annotation.Validated;

@Data
@Validated
public class SysConfigEditDto {
    @NotBlank
    private String name;
    @NotBlank
    private String value;
}

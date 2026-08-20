package com.moyz.adi.common.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;
import org.springframework.validation.annotation.Validated;

@Data
@Validated
public class KbDocumentToggleStatusReq {

    @NotBlank
    private String uuid;

    @NotNull
    private Boolean isEnabled;
}

package com.moyz.adi.common.dto;

import jakarta.validation.constraints.NotBlank;
import lombok.Data;
import org.hibernate.validator.constraints.Length;
import org.springframework.validation.annotation.Validated;

@Data
@Validated
public class ModifyPasswordReq {

    @NotBlank
    @Length(min = 6)
    private String oldPassword;

    @NotBlank
    @Length(min = 6)
    private String newPassword;
}

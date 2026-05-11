package com.moyz.adi.common.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Schema(name = "登录请求参数 | Login Request Parameters")
@Data
public class LoginReq {

    @NotBlank(message = "Email cannot be empty")
    String email;

    @NotBlank(message = "Password cannot be empty")
    String password;

    String captchaId;

    String captchaCode;
}

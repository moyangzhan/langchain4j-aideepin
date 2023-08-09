package com.moyz.adi.common.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Schema(name = "登录请求参数")
@Data
public class LoginReq {

    @NotBlank(message = "邮箱不能为空")
    String email;

    @NotBlank(message = "密码不能为空")
    String password;

    String captchaId;

    String captchaCode;
}

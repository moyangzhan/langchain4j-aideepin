package com.moyz.adi.common.dto;

import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.Min;
import lombok.Data;
import org.hibernate.validator.constraints.Length;
import org.springframework.validation.annotation.Validated;

@Schema(name = "注册请求参数 | Registration Request Parameters")
@Data
@Validated
public class RegisterReq {

    @Parameter(description = "邮箱 | Email")
    @Email
    private String email;

    @Parameter(description = "密码 | Password")
    @Min(6)
    private String password;

    @Parameter(description = "验证码ID | Captcha ID")
    @Length(min = 32)
    private String captchaId;

    @Parameter(description = "验证码 | Captcha Code")
    @Length(min = 4, max = 4)
    private String captchaCode;
}

package com.moyz.adi.common.dto;

import lombok.Data;

@Data
public class LoginResp {

    private String token;
    private String name;
    private String email;
    private String activeTime;
    private String captchaId;
}

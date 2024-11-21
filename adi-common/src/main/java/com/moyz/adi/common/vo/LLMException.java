package com.moyz.adi.common.vo;

import lombok.Data;

@Data
public class LLMException {
    private String code;
    private String message;
    private String type;
}

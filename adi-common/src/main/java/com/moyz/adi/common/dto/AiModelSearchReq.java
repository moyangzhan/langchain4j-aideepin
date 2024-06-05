package com.moyz.adi.common.dto;

import lombok.Data;

@Data
public class AiModelSearchReq {
    private String type;
    private String platform;
    private Boolean isEnable;
}

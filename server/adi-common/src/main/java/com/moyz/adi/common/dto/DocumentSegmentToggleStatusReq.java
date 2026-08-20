package com.moyz.adi.common.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

/**
 * 分段启停请求（与文档级 KbItemToggleStatusReq 对称）。
 */
@Data
public class DocumentSegmentToggleStatusReq {

    @NotBlank
    private String uuid;

    @NotNull
    private Boolean isEnabled;
}

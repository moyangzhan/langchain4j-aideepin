package com.moyz.adi.common.dto;

import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;
import org.springframework.validation.annotation.Validated;

@Data
@Validated
public class KbItemEditReq {

    private Long id;

    @Min(1)
    private Long kbId;

    private String kbUuid;

    private String uuid;

    @NotBlank
    private String title;

    private String brief;

    @NotBlank
    private String remark;
}

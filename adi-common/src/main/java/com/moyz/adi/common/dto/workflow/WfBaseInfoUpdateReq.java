package com.moyz.adi.common.dto.workflow;

import jakarta.validation.constraints.NotBlank;
import lombok.Data;
import org.springframework.validation.annotation.Validated;

@Validated
@Data
public class WfBaseInfoUpdateReq {
    @NotBlank
    private String uuid;
    @NotBlank
    private String title;
    private String remark;
    private Boolean isPublic;
}

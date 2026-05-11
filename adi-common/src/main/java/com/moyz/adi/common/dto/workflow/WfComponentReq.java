package com.moyz.adi.common.dto.workflow;

import jakarta.validation.constraints.NotBlank;
import lombok.Data;
import org.springframework.validation.annotation.Validated;

@Data
@Validated
public class WfComponentReq {
    private String uuid;
    @NotBlank(message = "Name cannot be empty")
    private String name;
    @NotBlank(message = "Title cannot be empty")
    private String title;
    private String remark;
    private Boolean isEnable;
    private Integer displayOrder;
}

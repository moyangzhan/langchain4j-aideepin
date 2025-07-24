package com.moyz.adi.common.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

import java.util.List;

@Data
public class ConvEditReq {

    @NotBlank
    private String title;

    private String remark;

    @Schema(title = "set the system message to ai, ig: you are a lawyer")
    private String aiSystemMessage;

    private Boolean understandContextEnable;

    private List<Long> mcpIds;

    private Integer answerContentType;

    private Boolean isAutoplayAnswer;
}

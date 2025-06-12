package com.moyz.adi.common.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;

@Data
public class ConvDto {

    private Long id;
    private String uuid;

    @NotBlank
    private String title;

    private String remark;

    private Integer tokens;

    @Schema(title = "set the system message to ai, ig: you are a lawyer")
    private String aiSystemMessage;

    private Boolean understandContextEnable;

    private List<Long> mcpIds;

    private LocalDateTime createTime;
    private LocalDateTime updateTime;
}

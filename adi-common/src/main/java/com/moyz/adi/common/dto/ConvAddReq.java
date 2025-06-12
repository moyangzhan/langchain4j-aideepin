package com.moyz.adi.common.dto;

import jakarta.validation.constraints.NotBlank;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.validation.annotation.Validated;

import java.util.List;

@AllArgsConstructor
@NoArgsConstructor
@Builder
@Data
@Validated
public class ConvAddReq {

    @NotBlank
    private String title;

    private String remark;

    private String aiSystemMessage;

    private List<Long> mcpIds;
}

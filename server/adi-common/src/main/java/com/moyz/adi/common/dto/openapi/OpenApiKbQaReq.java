package com.moyz.adi.common.dto.openapi;

import jakarta.validation.constraints.NotBlank;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class OpenApiKbQaReq {
    @NotBlank(message = "query is required")
    private String query;
    private String user;
    @Builder.Default
    private String responseMode = "streaming";
}

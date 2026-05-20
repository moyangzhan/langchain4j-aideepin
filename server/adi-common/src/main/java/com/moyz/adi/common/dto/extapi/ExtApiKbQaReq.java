package com.moyz.adi.common.dto.extapi;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.validation.constraints.NotBlank;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ExtApiKbQaReq {
    @NotBlank(message = "query is required")
    private String query;
    private String model;
    @JsonProperty("response_mode")
    @Builder.Default
    private String responseMode = "streaming";
}

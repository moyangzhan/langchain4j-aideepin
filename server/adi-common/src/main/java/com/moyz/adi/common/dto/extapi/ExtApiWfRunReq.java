package com.moyz.adi.common.dto.extapi;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Map;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ExtApiWfRunReq {
    private Map<String, Object> inputs;
    private String user;
    @Builder.Default
    private String responseMode = "streaming";
}

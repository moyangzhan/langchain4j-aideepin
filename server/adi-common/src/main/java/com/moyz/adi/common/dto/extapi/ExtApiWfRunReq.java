package com.moyz.adi.common.dto.extapi;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.node.ObjectNode;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ExtApiWfRunReq {
    private List<ObjectNode> inputs;
    @JsonProperty("response_mode")
    @Builder.Default
    private String responseMode = "streaming";
}

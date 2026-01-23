package com.moyz.adi.common.languagemodel.data;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class SiliconflowTtsReq {
    private String model;
    private String input;
    private String voice;
    @JsonProperty("response_format")
    private String responseFormat;
    private Boolean stream;
}

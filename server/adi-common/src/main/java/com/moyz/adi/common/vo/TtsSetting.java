package com.moyz.adi.common.vo;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

@Data
public class TtsSetting {
    @JsonProperty("synthesizer_side")
    private String synthesizerSide;
    @JsonProperty("model_name")
    private String modelName;
    private String platform;
}

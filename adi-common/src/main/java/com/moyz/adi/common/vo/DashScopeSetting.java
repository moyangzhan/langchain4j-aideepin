package com.moyz.adi.common.vo;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

@Data
public class DashScopeSetting {

    @JsonProperty("api_key")
    private String apiKey;
}

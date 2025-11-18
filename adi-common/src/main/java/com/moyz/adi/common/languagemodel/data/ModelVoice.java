package com.moyz.adi.common.languagemodel.data;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

@Data
public class ModelVoice {
    private String name;
    private String remark;
    @JsonProperty("param_name")
    private String paramName;
    private String lang;
}

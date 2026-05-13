package com.moyz.adi.common.vo;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

@Data
public class AsrSetting {
    @JsonProperty("model_name")
    private String modelName;
    private String platform;
    @JsonProperty("max_record_duration")
    private int maxRecordDuration;
    @JsonProperty("max_file_size")
    private int maxFileSize;
}

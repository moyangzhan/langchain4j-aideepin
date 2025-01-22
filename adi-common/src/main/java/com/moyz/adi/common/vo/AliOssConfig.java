package com.moyz.adi.common.vo;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

@Data
public class AliOssConfig {
    private String endpoint;
    @JsonProperty("access_key_id")
    private String accessKeyId;
    @JsonProperty("access_key_secret")
    private String accessKeySecret;
    @JsonProperty("bucket_name")
    private String bucketName;
}

package com.moyz.adi.common.dto;

import lombok.Data;

/**
 * LLM 调用记录搜索条件 | Search criteria for LLM call records
 */
@Data
public class LLMCallRecordSearchReq {

    private String userName;

    private String modelName;

    private String modelPlatform;

    private Integer sourceType;

    private Long[] requestTime;
}

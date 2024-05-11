package com.moyz.adi.common.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@NoArgsConstructor
@AllArgsConstructor
@Builder
@Data
public class AiSearchRecordResp {

    private String uuid;

    private String question;

    private SearchEngineResp searchEngineResp;

    private String prompt;

    private Integer promptTokens;

    private String answer;

    private Integer answerTokens;

    private String userUuid;

    private LocalDateTime createTime;

    private Long aiModelId;

    private String aiModelPlatform;
}

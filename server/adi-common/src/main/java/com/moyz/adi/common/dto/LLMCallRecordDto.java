package com.moyz.adi.common.dto;

import lombok.Data;

import java.time.LocalDateTime;

/**
 * LLM 调用记录列表项 | LLM call record list item
 * <p>在实体字段基础上补充关联的用户名，便于列表直接展示</p>
 * <p>Adds the associated user name on top of the entity fields for direct list display</p>
 */
@Data
public class LLMCallRecordDto {

    private Long id;

    private String uuid;

    private Integer sourceType;

    private Long sourceId;

    private Long userId;

    private String userName;

    private String modelPlatform;

    private String modelName;

    private Integer inputTokens;

    private Integer outputTokens;

    private Integer duration;

    private LocalDateTime requestTime;

    private LocalDateTime createTime;

    private LocalDateTime updateTime;
}

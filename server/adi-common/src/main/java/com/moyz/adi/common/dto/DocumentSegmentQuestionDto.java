package com.moyz.adi.common.dto;

import lombok.Data;

import java.time.LocalDateTime;

/**
 * qa 模式问题 DTO（列表展示用）
 */
@Data
public class DocumentSegmentQuestionDto {

    private Long id;

    private String uuid;

    private Long answerSegmentId;

    private Integer position;

    private String content;

    private Integer wordCount;

    private Integer hitCount;

    private LocalDateTime createTime;

    private LocalDateTime updateTime;
}

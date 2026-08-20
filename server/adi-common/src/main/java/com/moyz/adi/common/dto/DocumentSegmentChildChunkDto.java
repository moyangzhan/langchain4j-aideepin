package com.moyz.adi.common.dto;

import lombok.Data;

import java.time.LocalDateTime;

/**
 * parent_child 模式子块 DTO（列表展示用）
 */
@Data
public class DocumentSegmentChildChunkDto {

    private Long id;

    private String uuid;

    private Long parentSegmentId;

    private Integer position;

    private String content;

    private Integer wordCount;

    private Integer hitCount;

    private LocalDateTime createTime;

    private LocalDateTime updateTime;
}

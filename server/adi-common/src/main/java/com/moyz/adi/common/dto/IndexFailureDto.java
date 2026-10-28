package com.moyz.adi.common.dto;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;

/**
 * Latest failure of one index dimension (embedding/graphical) for the detail page failure list
 */
@Data
@Builder
public class IndexFailureDto {

    /**
     * 任务类型：embedding | graphical
     */
    private String taskType;

    /**
     * 净化后的失败原因（带 vectorize:/graph: 前缀，与宿主行同源）
     */
    private String failReason;

    /**
     * 面向用户的失败提示；存量行由读取侧按同一规则从 fail_reason 兜底转换
     */
    private String userMsg;

    /**
     * 失败落定时间（任务行 update_time）
     */
    private LocalDateTime updateTime;
}

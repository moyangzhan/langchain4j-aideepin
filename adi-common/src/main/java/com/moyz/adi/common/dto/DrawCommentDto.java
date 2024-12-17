package com.moyz.adi.common.dto;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;

@Data
@Builder
public class DrawCommentDto {
    private String uuid;
    private String userUuid;
    private String userName;
    private String drawUuid;
    private String remark;
    private LocalDateTime createTime;
}

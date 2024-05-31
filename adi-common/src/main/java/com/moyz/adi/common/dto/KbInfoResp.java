package com.moyz.adi.common.dto;

import lombok.Data;

import java.time.LocalDateTime;

@Data
public class KbInfoResp {
    private Long id;
    private String uuid;
    private String title;
    private String remark;
    private Boolean isPublic;
    private Integer starCount;
    private String ownerUuid;
    private String ownerName;
    private Integer itemCount;
    private Integer embeddingCount;
    private LocalDateTime createTime;
    private LocalDateTime updateTime;
}

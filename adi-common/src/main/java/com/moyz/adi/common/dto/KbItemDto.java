package com.moyz.adi.common.dto;

import lombok.Data;

import java.time.LocalDateTime;

@Data
public class KbItemDto {

    private Long kbId;

    private String kbUuid;

    private Long sourceFileId;

    private String uuid;

    private String title;

    private String brief;

    private String remark;

    private Boolean isEmbedded;

    private String sourceFileName;

    private String sourceFileUuid;

    private LocalDateTime createTime;

    private LocalDateTime updateTime;
}

package com.moyz.adi.common.dto;

import com.moyz.adi.common.enums.EmbeddingStatusEnum;
import com.moyz.adi.common.enums.GraphicalStatusEnum;
import lombok.Data;

import java.time.LocalDateTime;

@Data
public class KbItemDto {

    private Long kbId;

    private String kbUuid;

    private Long sourceFileId;

    private Long id;

    private String uuid;

    private String title;

    private String brief;

    private String remark;

    private EmbeddingStatusEnum embeddingStatus;

    private LocalDateTime embeddingStatusChangeTime;

    private GraphicalStatusEnum graphicalStatus;

    private LocalDateTime graphicalStatusChangeTime;

    private String sourceFileName;

    private String sourceFileUuid;

    private String sourceFileUrl;

    private LocalDateTime createTime;

    private LocalDateTime updateTime;
}

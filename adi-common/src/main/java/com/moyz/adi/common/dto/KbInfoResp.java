package com.moyz.adi.common.dto;

import lombok.Data;

@Data
public class KbInfoResp {
    private String uuid;
    private String title;
    private String remark;
    private Boolean isPublic;
    private Integer starCount;
    private String ownerUuid;
    private String ownerName;
    private Integer itemCount;
    private Integer embeddingCount;
}

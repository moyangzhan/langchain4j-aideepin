package com.moyz.adi.common.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class KbSearchReq {
    private String title;
    private Boolean isPublic;
    private Integer minItemCount;
    private Integer minEmbeddingCount;
    private Long[] createTime;
    private Long[] updateTime;
}

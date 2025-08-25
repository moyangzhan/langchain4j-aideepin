package com.moyz.adi.common.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
@AllArgsConstructor
public class AnswerMeta {
    private Integer tokens;
    private String uuid;
    private Boolean isRefEmbedding;
    private Boolean isRefGraph;
}

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
    @Builder.Default
    private Boolean isRefEmbedding = false;
    @Builder.Default
    private Boolean isRefGraph = false;
    @Builder.Default
    private Boolean isRefMemoryEmbedding = false;
}

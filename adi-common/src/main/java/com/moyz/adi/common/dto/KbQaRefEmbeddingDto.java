package com.moyz.adi.common.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@Data
@NoArgsConstructor
@AllArgsConstructor
public class KbQaRefEmbeddingDto {
    private String embeddingId;
    private String text;
}

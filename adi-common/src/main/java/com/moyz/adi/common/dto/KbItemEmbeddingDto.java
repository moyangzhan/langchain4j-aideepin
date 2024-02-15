package com.moyz.adi.common.dto;

import lombok.Data;

@Data
public class KbItemEmbeddingDto {
    private String embeddingId;

    private float[] embedding;

    private String text;
}

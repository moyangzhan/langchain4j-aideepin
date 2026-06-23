package com.moyz.adi.common.dto;

import com.fasterxml.jackson.databind.JsonNode;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@NoArgsConstructor
@AllArgsConstructor
@Data
public class KbItemEmbeddingDto {
    private String embeddingId;

    private float[] embedding;

    private String text;

    /**
     * 向量库存储的 metadata（JSONB）。仅角色记忆反查时使用，知识库反查路径可为 null。
     * <p>
     * Vector store metadata (JSONB). Used by character-memory reverse lookup to
     * distinguish semantic / episodic and surface structured fields; can be null
     * for knowledge-base lookups that don't need it.
     */
    private JsonNode metadata;
}

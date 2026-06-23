package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.databind.JsonNode;
import com.moyz.adi.common.base.JsonNodeTypeHandler;
import com.pgvector.PGvector;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * Episodic memory vector row, mirroring {@link CharacterMemoryEmbedding} but
 * mapped to the physically isolated {@code adi_character_episodic_memory_embedding}
 * table managed by langchain4j.
 * <p>
 * 情景记忆向量行实体，结构与 {@link CharacterMemoryEmbedding} 对称，映射到由 langchain4j
 * 管理的物理隔离表 {@code adi_character_episodic_memory_embedding}。
 */
@Data
@TableName(value = "adi_character_episodic_memory_embedding", autoResultMap = true)
@Schema(title = "角色-情景记忆-嵌入实体 | Character Episodic Memory Embedding Entity")
public class EpisodicMemoryEmbedding {

    @Schema(title = "embedding_id")
    @TableId(value = "embedding_id", type = IdType.AUTO)
    private String embeddingId;

    @Schema(title = "embedding")
    @TableField("embedding")
    private PGvector embedding;

    @Schema(title = "对应的文档 | Corresponding Document")
    @TableField("text")
    private String text;

    /**
     * JSONB metadata written by langchain4j on ingest (character_id / memory_type=episodic /
     * event_type / importance / created_at / source_msg_id).
     */
    @Schema(title = "metadata")
    @TableField(value = "metadata", typeHandler = JsonNodeTypeHandler.class)
    private JsonNode metadata;
}

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

@Data
@TableName(value = "adi_character_memory_embedding", autoResultMap = true)
@Schema(title = "角色-记忆-嵌入实体 | Character Memory Embedding Entity", description = "角色记忆嵌入表 | Character Memory Embedding Table")
public class CharacterMemoryEmbedding {

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
     * langchain4j 写入时存储的 JSONB metadata（character_id / memory_type /
     * event_type / importance / create_time / conv_msg_id 等）。读路径用于
     * 反查时区分语义/情景记忆并填充结构化字段。
     * <p>
     * JSONB metadata written by langchain4j on ingest. Read path uses it to
     * distinguish semantic vs episodic and pull structured fields for the UI.
     */
    @Schema(title = "metadata")
    @TableField(value = "metadata", typeHandler = JsonNodeTypeHandler.class)
    private JsonNode metadata;
}

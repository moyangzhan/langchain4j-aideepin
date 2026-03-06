package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.pgvector.PGvector;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Data
@TableName("adi_conversation_memory_embedding")
@Schema(title = "角色-记忆-嵌入实体", description = "角色记忆嵌入表")
public class ConversationMemoryEmbedding {

    @Schema(title = "embedding_id")
    @TableId(value = "embedding_id", type = IdType.AUTO)
    private String embeddingId;

    @Schema(title = "embedding")
    @TableField("embedding")
    private PGvector embedding;

    @Schema(title = "对应的文档")
    @TableField("text")
    private String text;
}

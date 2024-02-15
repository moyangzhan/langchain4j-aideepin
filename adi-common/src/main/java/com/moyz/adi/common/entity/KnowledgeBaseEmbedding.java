package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import com.pgvector.PGvector;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Data
@TableName("adi_knowledge_base_embedding")
@Schema(title = "知识库-嵌入实体", description = "知识库嵌入表")
public class KnowledgeBaseEmbedding extends BaseEntity {

    @Schema(title = "embedding uuid")
    @TableField("embedding")
    private String embeddingId;

    @Schema(title = "embedding")
    @TableField("embedding")
    private PGvector embedding;

    @Schema(title = "对应的文档")
    @TableField("text")
    private String text;
}

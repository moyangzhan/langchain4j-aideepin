package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

@Data
@TableName("adi_conversation_message_ref_embedding")
@Schema(title = "角色消息-知识库的向量-引用实体 | Character Message Embedding Reference Entity", description = "角色消息-知识库的向量-引用列表 | Character Message Embedding Reference List")
public class CharacterMessageRefEmbedding implements Serializable {

    private static final long serialVersionUID = 1L;

    @TableId(type = IdType.AUTO)
    private Long id;

    @Schema(title = "消息ID | Message ID")
    @TableField("message_id")
    private Long messageId;

    @Schema(title = "向量id | Embedding ID")
    @TableField("embedding_id")
    private String embeddingId;

    @Schema(title = "分数 | Score")
    @TableField("score")
    private Double score;

    @Schema(title = "提问用户id | Question User ID")
    @TableField("user_id")
    private Long userId;
}

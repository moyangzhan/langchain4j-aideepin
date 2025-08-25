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
@Schema(title = "会话消息-知识库的向量-引用实体", description = "会话消息-知识库的向量-引用列表")
public class ConversationMessageRefEmbedding implements Serializable {

    private static final long serialVersionUID = 1L;

    @TableId(type = IdType.AUTO)
    private Long id;

    @Schema(title = "消息ID")
    @TableField("message_id")
    private Long messageId;

    @Schema(title = "向量id")
    @TableField("embedding_id")
    private String embeddingId;

    @Schema(title = "分数")
    @TableField("score")
    private Double score;

    @Schema(title = "提问用户id")
    @TableField("user_id")
    private Long userId;
}

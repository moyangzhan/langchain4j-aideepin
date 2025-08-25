package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

@Data
@TableName("adi_conversation_message_ref_graph")
@Schema(title = "会话消息-知识库-图谱引用", description = "会话消息-知识库-图谱引用列表")
public class ConversationMessageRefGraph implements Serializable {

    private static final long serialVersionUID = 1L;

    @TableId(type = IdType.AUTO)
    private Long id;

    @Schema(title = "消息ID")
    @TableField("message_id")
    private Long messageId;

    @Schema(title = "用户问题解析出来的实体")
    @TableField("entities_from_question")
    private String entitiesFromQuestion;

    @Schema(title = "从图数据库中查找得到的图谱")
    @TableField("graph_from_store")
    private String graphFromStore;

    @Schema(title = "提问用户id")
    @TableField("user_id")
    private Long userId;
}

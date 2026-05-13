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
@Schema(title = "会话消息-知识库-图谱引用 | Conversation Message Knowledge Graph Reference", description = "会话消息-知识库-图谱引用列表 | Conversation Message Knowledge Graph Reference List")
public class ConversationMessageRefGraph implements Serializable {

    private static final long serialVersionUID = 1L;

    @TableId(type = IdType.AUTO)
    private Long id;

    @Schema(title = "消息ID | Message ID")
    @TableField("message_id")
    private Long messageId;

    @Schema(title = "LLM解析出来的图谱 | Graph Parsed by LLM")
    @TableField("graph_from_llm")
    private String graphFromLlm;

    @Schema(title = "从图数据库中查找得到的图谱 | Graph Retrieved from Graph Database")
    @TableField("graph_from_store")
    private String graphFromStore;

    @Schema(title = "提问用户id | Question User ID")
    @TableField("user_id")
    private Long userId;
}

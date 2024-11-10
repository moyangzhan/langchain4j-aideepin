package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * <p>
 *
 * </p>
 *
 * @author moyz
 * @since 2023-04-11
 */
@Data
@TableName("adi_conversation_message")
@Schema(title = "ConversationMessage对象")
public class ConversationMessage extends BaseEntity {

    @Schema(title = "消息的uuid")
    @TableField("uuid")
    private String uuid;

    @Schema(title = "父级消息id")
    @TableField("parent_message_id")
    private Long parentMessageId;

    @Schema(title = "对话id")
    @TableField("conversation_id")
    private Long conversationId;

    @Schema(title = "对话uuid")
    @TableField("conversation_uuid")
    private String conversationUuid;

    @Schema(title = "用户id")
    @TableField("user_id")
    private Long userId;

    @Schema(title = "对话的消息")
    @TableField("remark")
    private String remark;

    @Schema(title = "产生该消息的角色：1: 用户,2:系统,3:助手")
    @TableField("message_role")
    private Integer messageRole;

    @Schema(title = "消耗的token数量")
    @TableField("tokens")
    private Integer tokens;

    @Schema(name = "上下文理解中携带的消息对数量（提示词及回复）")
    @TableField("understand_context_msg_pair_num")
    private Integer understandContextMsgPairNum;

    @Schema(name = "adi_ai_model id")
    @TableField("ai_model_id")
    private Long aiModelId;

    @Schema(name = "附件列表")
    @TableField("attachments")
    private String attachments;
}

package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * <p>
 *
 * </p>
 *
 * @author moyz
 * @since 2023-04-11
 */
@EqualsAndHashCode(callSuper = true)
@Data
@TableName("adi_conversation_message")
@Schema(title = "ConversationMessage对象")
public class ConversationMessage extends BaseEntity {

    @Schema(title = "消息的uuid | Message UUID")
    @TableField("uuid")
    private String uuid;

    @Schema(title = "父级消息id | Parent Message ID")
    @TableField("parent_message_id")
    private Long parentMessageId;

    @Schema(title = "对话id | Conversation ID")
    @TableField("conversation_id")
    private Long conversationId;

    @Schema(title = "对话uuid | Conversation UUID")
    @TableField("conversation_uuid")
    private String conversationUuid;

    @Schema(title = "用户id | User ID")
    @TableField("user_id")
    private Long userId;

    @Schema(title = "原始的对话消息，如用户输入的问题，AI产生的回答 | Original Conversation Message (e.g., User Question, AI Answer)")
    @TableField("remark")
    private String remark;

    @Schema(title = "处理过的有效的对话消息，如 1.提供给LLM的内容：用户输入的问题+关联的知识库；2.显示在用户面前的答案：AI产生的回答经过合规校验及过滤、个性化调整后的内容 | Processed Valid Message (1. Content for LLM: User Question + Related KB; 2. Answer Displayed: AI Response After Compliance Check and Filtering)")
    @TableField("processed_remark")
    private String processedRemark;

    @Schema(title = "思考内容 | Thinking Content")
    @TableField("thinking_content")
    private String thinkingContent;

    @Schema(title = "语音聊天时产生的音频文件uuid(对应adi_file.uuid) | Audio File UUID in Voice Chat (Corresponds to adi_file.uuid)")
    @TableField("audio_uuid")
    private String audioUuid;

    @Schema(title = "语音聊天时产生的音频时长，单位秒 | Audio Duration in Voice Chat (seconds)")
    @TableField("audio_duration")
    private Integer audioDuration;

    @Schema(title = "产生该消息的角色：1: 用户,2:系统,3:助手 | Message Role: 1=User, 2=System, 3=Assistant")
    @TableField("message_role")
    private Integer messageRole;

    @Schema(title = "消耗的token数量 | Token Count Consumed")
    @TableField("tokens")
    private Integer tokens;

    @Schema(name = "上下文理解中携带的消息对数量（提示词及回复） | Message Pair Count in Context Understanding (Prompt and Reply)")
    @TableField("understand_context_msg_pair_num")
    private Integer understandContextMsgPairNum;

    @Schema(name = "adi_ai_model id")
    @TableField("ai_model_id")
    private Long aiModelId;

    @Schema(name = "附件列表 | Attachment List")
    @TableField("attachments")
    private String attachments;

    @Schema(title = "响应内容类型：2：文本，3：音频 | Response Content Type: 2=Text, 3=Audio")
    @TableField("content_type")
    private Integer contentType;

    @Schema(title = "是否引用了向量库知识 | Whether Embedding Knowledge is Referenced")
    @TableField(value = "is_ref_embedding")
    private Boolean isRefEmbedding;

    @Schema(title = "是否引用了图谱库知识 | Whether Graph Knowledge is Referenced")
    @TableField(value = "is_ref_graph")
    private Boolean isRefGraph;

    @Schema(title = "是否引用记忆向量库 | Whether Memory Embedding is Referenced")
    @TableField(value = "is_ref_memory_embedding")
    private Boolean isRefMemoryEmbedding;

}

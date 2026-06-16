package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

@EqualsAndHashCode(callSuper = true)
@Data
@TableName("adi_character_message")
@Schema(title = "CharacterMessage对象")
public class CharacterMessage extends BaseEntity {

    @Schema(title = "消息的uuid | Message UUID")
    @TableField("uuid")
    private String uuid;

    @Schema(title = "父级消息id | Parent Message ID")
    @TableField("parent_message_id")
    private Long parentMessageId;

    @Schema(title = "角色id | Character ID")
    @TableField("character_id")
    private Long characterId;

    @Schema(title = "角色uuid | Character UUID")
    @TableField("character_uuid")
    private String characterUuid;

    @Schema(title = "用户id | User ID")
    @TableField("user_id")
    private Long userId;

    @Schema(title = "原始的对话消息，如用户输入的问题，AI产生的回答 | Original Message (e.g., User Question, AI Answer)")
    @TableField("remark")
    private String remark;

    @Schema(title = "处理过的有效的对话消息 | Processed Valid Message")
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

    @Schema(name = "上下文理解中携带的消息对数量 | Message Pair Count in Context Understanding")
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

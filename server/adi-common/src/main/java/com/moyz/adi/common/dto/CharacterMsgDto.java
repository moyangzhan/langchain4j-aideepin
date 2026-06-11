package com.moyz.adi.common.dto;

import com.fasterxml.jackson.annotation.JsonIgnore;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;

@Data
public class CharacterMsgDto {

    @JsonIgnore
    private Long id;

    @Schema(title = "消息的uuid | Message UUID")
    private String uuid;

    @Schema(title = "父级消息id | Parent Message ID")
    private Long parentMessageId;

    @Schema(title = "对话的消息 | Character Message")
    private String remark;

    @Schema(title = "思考的内容 | Thinking Content")
    private String thinkingContent;

    @Schema(title = "音频文件uuid | Audio File UUID")
    private String audioUuid;

    @Schema(title = "音频文件Url | Audio File URL")
    private String audioUrl;

    @Schema(title = "语音聊天时产生的音频时长，单位秒 | Audio Duration in Voice Chat (seconds)")
    private Integer audioDuration;

    @Schema(title = "产生该消息的角色：1: 用户,2:系统,3:助手 | Message Role: 1=User, 2=System, 3=Assistant")
    private Integer messageRole;

    @Schema(title = "消耗的token数量 | Token Count Consumed")
    private Integer tokens;

    @Schema(title = "输入token数量 | Input Token Count")
    private Integer inputTokens;

    @Schema(title = "输出token数量 | Output Token Count")
    private Integer outputTokens;

    @Schema(title = "调用耗时（毫秒） | Call Duration (ms)")
    private Integer duration;

    @Schema(title = "创建时间 | Create Time")
    private LocalDateTime createTime;

    @Schema(title = "model id")
    private Long aiModelId;

    @Schema(title = "model platform name")
    private String aiModelPlatform;

    @Schema(title = "附件地址 | Attachment URLs")
    private List<String> attachmentUrls;

    @Schema(title = "子级消息（一般指的是AI的响应） | Child Messages (Usually AI Responses)")
    private List<CharacterMsgDto> children;

    @Schema(title = "内容格式，2：文本；3：音频 | Content Format: 2=Text, 3=Audio")
    private Integer contentType;

    @Schema(title = "是否引用了向量库知识 | Whether Embedding Knowledge is Referenced")
    private Boolean isRefEmbedding;

    @Schema(title = "是否引用了图谱库知识 | Whether Graph Knowledge is Referenced")
    private Boolean isRefGraph;

    @Schema(title = "是否引用了记忆 | Whether Memory is Referenced")
    private Boolean isRefMemoryEmbedding;
}

package com.moyz.adi.common.dto;

import com.baomidou.mybatisplus.annotation.TableField;
import com.fasterxml.jackson.annotation.JsonIgnore;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;

@Data
public class ConvMsgResp {

    @JsonIgnore
    private Long id;

    @Schema(title = "消息的uuid")
    private String uuid;

    @Schema(title = "父级消息id")
    private Long parentMessageId;

    @Schema(title = "对话的消息")
    @TableField("content")
    private String content;

    @Schema(title = "产生该消息的角色：1: 用户,2:系统,3:助手")
    private String messageRole;

    @Schema(title = "消耗的token数量")
    private Integer tokens;

    @Schema(title = "创建时间")
    private LocalDateTime createTime;

    @Schema(title = "子级消息（一般指的是AI的响应）")
    private List<ConvMsgResp> children;
}

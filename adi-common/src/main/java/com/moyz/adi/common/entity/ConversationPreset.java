package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Data
@TableName("adi_conversation_preset")
@Schema(title = "预设对话实体", description = "预设对话表")
public class ConversationPreset extends BaseEntity {
    private String uuid;
    private String title;
    private String remark;
    private String aiSystemMessage;
}

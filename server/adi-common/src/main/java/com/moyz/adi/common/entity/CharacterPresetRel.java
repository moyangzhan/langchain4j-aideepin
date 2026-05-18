package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@TableName("adi_conversation_preset_rel")
@Schema(title = "预设角色与用户角色关系实体 | Character Preset Relation Entity", description = "预设角色与用户角色关系表 | Character Preset Relation Table")
public class CharacterPresetRel extends BaseEntity {
    private String uuid;
    private Long userId;
    @TableField("preset_conv_id")
    private Long presetCharacterId;
    @TableField("user_conv_id")
    private Long userCharacterId;
}

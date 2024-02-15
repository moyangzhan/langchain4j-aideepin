package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Data
@TableName("adi_prompt")
@Schema(title = "提示词实体")
public class Prompt extends BaseEntity {

    @Schema(title = "用户id")
    @TableField(value = "user_id")
    private Long userId;

    @Schema(title = "标题")
    @TableField(value = "act")
    private String act;

    @Schema(title = "内容")
    @TableField(value = "prompt")
    private String prompt;

}

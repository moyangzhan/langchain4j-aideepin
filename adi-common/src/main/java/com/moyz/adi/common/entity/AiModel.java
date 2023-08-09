package com.moyz.adi.common.entity;

import com.moyz.adi.common.enums.AiModelStatus;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Data
@TableName("adi_ai_model")
@Schema(title = "AiModel对象", description = "AI模型表")
public class AiModel extends BaseEntity {

    @Schema(title = "模型名称")
    @TableField("name")
    private String name;

    @Schema(title = "说明")
    @TableField("remark")
    private String remark;

    @Schema(title = "状态(1:正常使用,2:不可用)")
    @TableField("model_status")
    private AiModelStatus modelStatus;
}

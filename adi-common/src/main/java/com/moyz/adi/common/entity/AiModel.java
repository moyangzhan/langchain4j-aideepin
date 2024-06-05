package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Data
@TableName("adi_ai_model")
@Schema(title = "AiModel对象", description = "AI模型表")
public class AiModel extends BaseEntity {

    @Schema(title = "模型类型:text,image,embedding,rerank")
    @TableField("type")
    private String type;

    @Schema(title = "模型名称")
    @TableField("name")
    private String name;

    @Schema(title = "模型所属平台")
    @TableField("platform")
    private String platform;

    @Schema(title = "说明")
    @TableField("remark")
    private String remark;

    @Schema(title = "状态(1:正常使用,0:不可用)")
    @TableField("is_enable")
    private Boolean isEnable;

    @Schema(title = "上下文长度")
    @TableField("context_window")
    private Integer contextWindow;
}

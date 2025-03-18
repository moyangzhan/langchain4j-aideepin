package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

@Data
@EqualsAndHashCode(callSuper = true)
@TableName(value = "adi_workflow_component", autoResultMap = true)
@Schema(title = "工作流组件")
public class WorkflowComponent extends BaseEntity {

    @Serial
    private static final long serialVersionUID = 1L;

    @TableField("uuid")
    private String uuid;

    @TableField("name")
    private String name;

    @TableField("title")
    private String title;

    @TableField("remark")
    private String remark;

    @TableField("is_enable")
    private Boolean isEnable;
}

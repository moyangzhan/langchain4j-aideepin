package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Data
@TableName("adi_sys_config")
@Schema(title = "系统配置表")
public class SysConfig extends BaseEntity {

    @Schema(title = "配置名称")
    @TableField("name")
    private String name;

    @Schema(title = "配置项的值")
    private String value;

    @Schema(title = "是否删除（0：未删除，1：已删除）")
    @TableField(value = "is_delete")
    private Boolean isDelete;
}

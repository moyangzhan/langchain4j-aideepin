package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Data
@TableName("adi_sys_config")
@Schema(title = "系统配置表 | System Config Table")
public class SysConfig extends BaseEntity {

    @Schema(title = "配置名称 | Config Name")
    @TableField("name")
    private String name;

    @Schema(title = "配置项的值 | Config Value")
    private String value;

}

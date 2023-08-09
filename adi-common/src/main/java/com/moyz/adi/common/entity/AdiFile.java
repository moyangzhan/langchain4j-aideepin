package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Data
@TableName("adi_file")
@Schema(title = "文件表")
public class AdiFile extends BaseEntity {
    @Schema(title = "用户id")
    @TableField(value = "user_id")
    private Long userId;

    @Schema(title = "name")
    @TableField(value = "name")
    private String name;

    @Schema(title = "uuid")
    @TableField(value = "uuid")
    private String uuid;

    @Schema(title = "md5")
    @TableField(value = "md5")
    private String md5;

    @Schema(title = "file extension")
    @TableField(value = "ext")
    private String ext;

    @Schema(title = "路径")
    @TableField(value = "path")
    private String path;

    @Schema(title = "引用数量")
    @TableField(value = "ref_count")
    private Integer refCount;

    @Schema(title = "是否删除（0：未删除，1：已删除）")
    @TableField(value = "is_delete")
    private Boolean isDelete;
}

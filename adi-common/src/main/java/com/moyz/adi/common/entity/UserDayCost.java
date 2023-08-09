package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Data
@TableName("adi_user_day_cost")
@Schema(title = "用户每天使用量")
public class UserDayCost extends BaseEntity {
    @Schema(title = "用户id")
    @TableField(value = "user_id")
    private Long userId;

    @Schema(title = "日期")
    @TableField(value = "day")
    private Integer day;

    @Schema(title = "请求量")
    @TableField(value = "requests")
    private Integer requests;

    @Schema(title = "token数量")
    @TableField(value = "tokens")
    private Integer tokens;

    @Schema(title = "The number of generated images")
    @TableField(value = "images_number")
    private Integer imagesNumber;

    @Schema(title = "secret key type(1:system secret key,2:custom secret key)")
    @TableField(value = "secret_key_type")
    private Integer secretKeyType;
}

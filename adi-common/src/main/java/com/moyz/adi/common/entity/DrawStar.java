package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

@EqualsAndHashCode(callSuper = true)
@Data
@TableName("adi_draw_star")
@Schema(title = "Favorite draws", description = "Favorite draws")
public class DrawStar extends BaseEntity {

    @TableField("user_id")
    private Long userId;

    @TableField("draw_id")
    private Long drawId;
}

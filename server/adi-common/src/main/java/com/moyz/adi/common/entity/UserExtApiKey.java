package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

@EqualsAndHashCode(callSuper = true)
@Data
@TableName(value = "adi_user_ext_api_key")
@Schema(title = "用户级External API Key实体 | User-level External API Key Entity")
public class UserExtApiKey extends BaseEntity {

    @Schema(title = "用户ID | User ID")
    @TableField(value = "user_id")
    private Long userId;

    @Schema(title = "资源类型：draw, mcp | Resource type: draw, mcp")
    @TableField(value = "resource_type")
    private String resourceType;

    @Schema(title = "加密后的API Key | Encrypted API Key")
    @TableField(value = "api_key")
    private String apiKey;
}

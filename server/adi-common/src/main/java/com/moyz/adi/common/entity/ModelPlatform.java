package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

@Data
@EqualsAndHashCode(callSuper = true)
@TableName(value = "adi_model_platform", autoResultMap = true)
@Schema(title = "ModelPlatform对象 | ModelPlatform Object", description = "模型平台表 | Model Platform Table")
public class ModelPlatform extends BaseEntity {

    @Schema(title = "名称 | Name")
    @TableField("name")
    private String name;

    @Schema(title = "标题(更易理解记忆的名称) | Title (A More Memorable Name)")
    @TableField("title")
    private String title;

    @Schema(title = "说明 | Description")
    @TableField("remark")
    private String remark;

    @Schema(title = "base url")
    @TableField("base_url")
    private String baseUrl;

    @Schema(title = "api key")
    @TableField("api_key")
    private String apiKey;

    @Schema(title = "secret key, 可选 | Secret Key (Optional)")
    @TableField("secret_key")
    private String secretKey;

    @Schema(title = "是否开启代理，代理的详细配置在全局配置里，路径：adi.proxy | Enable Proxy (Proxy Config in Global Settings: adi.proxy)")
    @TableField("is_proxy_enable")
    private Boolean isProxyEnable;

    @Schema(title = "平台接口是否兼容OpenAI API | Is OpenAI API Compatible")
    @TableField("is_openai_api_compatible")
    private Boolean isOpenaiApiCompatible;
}

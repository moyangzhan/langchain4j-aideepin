package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import com.moyz.adi.common.config.McpCustomizedParamDefinitionTypeHandler;
import com.moyz.adi.common.config.McpPresetParamTypeHandler;
import com.moyz.adi.common.dto.mcp.McpCommonParam;
import com.moyz.adi.common.dto.mcp.McpCustomizedParamDefinition;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.apache.ibatis.type.JdbcType;

import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Data
@TableName(value = "adi_mcp", autoResultMap = true)
@Schema(title = "mcp服务实体")
public class Mcp extends BaseEntity {

    @Schema(title = "uuid")
    @TableField(value = "uuid")
    private String uuid;

    @Schema(title = "标题")
    @TableField(value = "title")
    private String title;

    @Schema(title = "传输类型(1:sse,2:stdio)")
    @TableField(value = "transport_type")
    private String transportType;

    @Schema(title = "sse url")
    @TableField(value = "sse_url")
    private String sseUrl;

    @Schema(title = "sse超时时间")
    @TableField(value = "sse_timeout")
    private Integer sseTimeout;

    @Schema(title = "stdio命令")
    @TableField(value = "stdio_command")
    private String stdioCommand;

    @Schema(title = "stdio参数")
    @TableField(value = "stdio_arg")
    private String stdioArg;

    @Schema(title = "由系统管理员预设的参数")
    @TableField(value = "preset_params", jdbcType = JdbcType.ARRAY, typeHandler = McpPresetParamTypeHandler.class)
    private List<McpCommonParam> presetParams;

    @Schema(title = "待用户设置的参数定义,用户设置后与common_params合并做为mcp的启动参数")
    @TableField(value = "customized_param_definitions", jdbcType = JdbcType.ARRAY, typeHandler = McpCustomizedParamDefinitionTypeHandler.class)
    private List<McpCustomizedParamDefinition> customizedParamDefinitions;

    @Schema(title = "安装类型(1:docker、2:local、3:remote、4:wasm)")
    @TableField(value = "install_type")
    private String installType;

    @Schema(title = "网址")
    @TableField(value = "website")
    private String website;

    @Schema(title = "描述")
    @TableField(value = "remark")
    private String remark;

    @Schema(title = "是否启用")
    @TableField(value = "is_enable")
    private Boolean isEnable;
}

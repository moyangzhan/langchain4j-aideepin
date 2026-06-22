package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.moyz.adi.common.base.ObjectNodeTypeHandler;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.apache.commons.lang3.StringUtils;
import org.apache.ibatis.type.JdbcType;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

@Data
@EqualsAndHashCode(callSuper = true)
@TableName(value = "adi_ai_model", autoResultMap = true)
@Schema(title = "AiModel对象 | AiModel Object", description = "AI模型表 | AI Model Table")
public class AiModel extends BaseEntity {

    @Schema(title = "模型类型:text,image,embedding,rerank | Model Type: text, image, embedding, rerank")
    @TableField("type")
    private String type;

    @Schema(title = "模型名称 | Model Name")
    @TableField("name")
    private String name;

    @Schema(title = "模型标题(更易理解记忆的名称) | Model Title (A More Memorable Name)")
    @TableField("title")
    private String title;

    @Schema(title = "模型所属平台 | Model Platform")
    @TableField("platform")
    private String platform;

    @Schema(title = "模型配置 | Model Configuration")
    @TableField("setting")
    private String setting;

    @Schema(title = "说明 | Description")
    @TableField("remark")
    private String remark;

    @Schema(title = "是否免费(true:免费,false:收费) | Is Free (true: Free, false: Paid)")
    @TableField("is_free")
    private Boolean isFree;

    @Schema(title = "状态(1:正常使用,0:不可用) | Status (1: Available, 0: Unavailable)")
    @TableField("is_enable")
    private Boolean isEnable;

    @Schema(title = "上下文长度 | Context Window Size")
    @TableField("context_window")
    private Integer contextWindow;

    @Schema(title = "最大输入长度 | Max Input Length")
    @TableField("max_input_tokens")
    private Integer maxInputTokens;

    @Schema(title = "最大输出长度 | Max Output Length")
    @TableField("max_output_tokens")
    private Integer maxOutputTokens;

    @Schema(title = "输入类型 | Input Types")
    @TableField("input_types")
    private String inputTypes;

    @Schema(title = "支持的输出格式: text,json_object | Supported Output Formats: text, json_object")
    @TableField("response_format_types")
    private String responseFormatTypes;

    @Schema(title = "属性 | Properties")
    @TableField(value = "properties", jdbcType = JdbcType.JAVA_OBJECT, typeHandler = ObjectNodeTypeHandler.class)
    private ObjectNode properties;

    @Schema(title = "是否推理模型 | Is Reasoner Model")
    @TableField("is_reasoner")
    private Boolean isReasoner;

    @Schema(title = "思考过程是否可以关闭 | Is Thinking Process Closable")
    @TableField("is_thinking_closable")
    private Boolean isThinkingClosable;

    @Schema(title = "是否支持web搜索 | Is Web Search Supported")
    @TableField("is_support_web_search")
    private Boolean isSupportWebSearch;

    /**
     * 从 properties 中获取 max_images 属性
     * 如果未配置，默认返回 1
     */
    public int getMaxImages() {
        if (properties == null || properties.isEmpty()) {
            return 1;
        }
        JsonNode maxImages = properties.get("max_images");
        if (maxImages == null || maxImages.isNull()) {
            return 1;
        }
        try {
            return maxImages.asInt(1);
        } catch (Exception e) {
            return 1;
        }
    }

    /**
     * 从 properties.sizes 中提取可选尺寸的 value 列表,用于后端校验前端传入的 size 是否合法。
     * <p>
     * Extract the list of valid size values from properties.sizes; used to validate the size
     * submitted by the frontend against the model's allowed options.
     */
    public List<String> getSizes() {
        if (properties == null || properties.isEmpty()) {
            return Collections.emptyList();
        }
        JsonNode sizes = properties.get("sizes");
        if (sizes == null || !sizes.isArray() || sizes.isEmpty()) {
            return Collections.emptyList();
        }
        List<String> result = new ArrayList<>();
        for (JsonNode item : sizes) {
            JsonNode value = item.get("value");
            if (value != null && !value.isNull()) {
                result.add(value.asText());
            }
        }
        return result;
    }
}

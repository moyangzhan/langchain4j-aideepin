package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * LLM 调用记录，统一记录全站所有 LLM 调用的资源消耗 | LLM call record, unified resource consumption tracking
 */
@EqualsAndHashCode(callSuper = true)
@Data
@TableName("adi_llm_call_record")
public class LLMCallRecord extends BaseEntity {

    @Schema(title = "记录 UUID | Record UUID")
    @TableField("uuid")
    private String uuid;

    @Schema(title = "来源类型：0=unknown 1=character_chat 2=knowledge_base_qa 3=knowledge_base_ingest 4=workflow_node 5=agent")
    @TableField("source_type")
    private Integer sourceType;

    @Schema(title = "源表主键 ID | Source record primary key")
    @TableField("source_id")
    private Long sourceId;

    @Schema(title = "用户 ID | User ID")
    @TableField("user_id")
    private Long userId;

    @Schema(title = "模型平台名称 | Model platform name")
    @TableField("model_platform")
    private String modelPlatform;

    @Schema(title = "模型名称 | Model name")
    @TableField("model_name")
    private String modelName;

    @Schema(title = "输入 token 数量 | Input token count")
    @TableField("input_tokens")
    private Integer inputTokens;

    @Schema(title = "输出 token 数量 | Output token count")
    @TableField("output_tokens")
    private Integer outputTokens;

    @Schema(title = "调用耗时（毫秒） | Call duration in ms")
    @TableField("duration")
    private Integer duration;

    @Schema(title = "请求发起时间 | Request start time")
    @TableField("request_time")
    private java.time.LocalDateTime requestTime;
}

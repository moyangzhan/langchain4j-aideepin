package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.moyz.adi.common.base.JsonNodeTypeHandler;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.apache.ibatis.type.JdbcType;

import java.io.Serial;

@Data
@EqualsAndHashCode(callSuper = true)
@TableName(value = "adi_workflow_runtime", autoResultMap = true)
@Schema(title = "工作流运行时 | Workflow runtime")
public class WorkflowRuntime extends BaseEntity {

    @Serial
    private static final long serialVersionUID = 1L;

    @TableField("uuid")
    private String uuid;

    @TableField("user_id")
    private Long userId;

    @TableField("workflow_id")
    private Long workflowId;

    @TableField(value = "input", jdbcType = JdbcType.JAVA_OBJECT, typeHandler = JsonNodeTypeHandler.class)
    private ObjectNode input;

    @TableField(value = "\"output\"", jdbcType = JdbcType.JAVA_OBJECT, typeHandler = JsonNodeTypeHandler.class)
    private ObjectNode output;

    @TableField("status")
    private Integer status;

    @TableField("status_remark")
    private String statusRemark;

    /**
     * Total input tokens aggregated from LLM-typed nodes. Written at terminal status (success / fail / waiting_input).
     */
    @TableField("input_tokens")
    private Integer inputTokens;

    /**
     * Total output tokens aggregated from LLM-typed nodes. Written at terminal status.
     */
    @TableField("output_tokens")
    private Integer outputTokens;

    /**
     * Total run duration in milliseconds aggregated from all nodes. Written at terminal status.
     */
    @TableField("duration")
    private Integer duration;

    /**
     * Serialized langgraph4j checkpoint captured at human-feedback interruption.
     * select=false keeps the blob out of every default SELECT (paging / listing); only the resume-rebuild path queries it explicitly.
     * Non-NULL only while status = waiting_input — written with the same UPDATE that sets waiting_input, cleared at terminal status / timeout expiry.
     * 中断快照 blob：默认 SELECT 不拉取，仅 resume 重建路径显式查询；非空仅存在于等待输入期间
     */
    @TableField(value = "checkpoint", select = false)
    private byte[] checkpoint;
}

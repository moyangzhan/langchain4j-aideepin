package com.moyz.adi.common.workflow.metrics;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.io.Serial;

/**
 * Agent 节点可观测指标
 * <p>
 * Agent node observability metrics — extends LLMMetrics with RAG and Character info.
 * </p>
 */
@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@AllArgsConstructor
public class AgentMetrics extends LLMMetrics {

    @Serial
    private static final long serialVersionUID = 1L;

    /**
     * 知识检索结果数量
     * <p>
     * Knowledge retrieval result count.
     * </p>
     */
    private Integer retrievalCount;

    /**
     * 关联的角色 UUID
     * <p>
     * Associated Character UUID.
     * </p>
     */
    private String characterUuid;
}

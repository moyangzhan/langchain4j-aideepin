package com.moyz.adi.common.workflow.metrics;

import com.moyz.adi.common.workflow.NodeExecutionMetrics;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.io.Serial;

/**
 * 知识检索节点可观测指标 | Knowledge retrieval node observability metrics
 */
@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@AllArgsConstructor
public class KnowledgeRetrievalMetrics extends NodeExecutionMetrics {

    @Serial
    private static final long serialVersionUID = 1L;

    private Integer retrievalCount;
}

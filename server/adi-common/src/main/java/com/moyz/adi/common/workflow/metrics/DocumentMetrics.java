package com.moyz.adi.common.workflow.metrics;

import com.moyz.adi.common.workflow.NodeExecutionMetrics;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.io.Serial;

/**
 * 文档提取节点可观测指标 | Document extractor node observability metrics
 */
@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@AllArgsConstructor
public class DocumentMetrics extends NodeExecutionMetrics {

    @Serial
    private static final long serialVersionUID = 1L;

    private Integer fileCount;
    private Integer extractedCharCount;
}

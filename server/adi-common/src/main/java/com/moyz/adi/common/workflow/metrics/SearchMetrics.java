package com.moyz.adi.common.workflow.metrics;

import com.moyz.adi.common.workflow.NodeExecutionMetrics;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.io.Serial;

/**
 * 搜索节点可观测指标 | Search node observability metrics
 */
@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@AllArgsConstructor
public class SearchMetrics extends NodeExecutionMetrics {

    @Serial
    private static final long serialVersionUID = 1L;

    private Integer searchResultCount;
}

package com.moyz.adi.common.dto.workflow;

/**
 * Aggregated per-runtime statistics rolled up from node metadata. Tokens come from LLM-typed
 * nodes; duration is the SUM of per-node durations (note: this is sum-of-CPU-time, not wall-clock —
 * for parallel branches the wall-clock total is computed separately at the runtime level).
 *
 * @param inputTokens  total input tokens
 * @param outputTokens total output tokens
 * @param duration     sum of per-node durations in milliseconds (NOT wall-clock)
 */
public record WfRuntimeMetricsSummary(long inputTokens, long outputTokens, long duration) {
}

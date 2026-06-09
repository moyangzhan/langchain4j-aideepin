package com.moyz.adi.common.enums;

/**
 * 模型健康状态
 * <p>
 * Model health status.
 * </p>
 */
public enum ModelHealthStatus {
    /**
     * 可用 — 探测成功
     */
    HEALTHY,
    /**
     * 不可用 — 连续 3 次探测失败
     */
    UNHEALTHY,
    /**
     * 未探测 — 刚启动 / 刚添加的模型
     */
    UNKNOWN
}

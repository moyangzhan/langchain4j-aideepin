package com.moyz.adi.common.enums;

/**
 * Model health status.
 */
public enum ModelHealthStatus {
    /**
     * Available — probe succeeded or not yet probed.
     */
    HEALTHY,
    /**
     * Unavailable — 3 consecutive probe failures.
     */
    UNHEALTHY
}

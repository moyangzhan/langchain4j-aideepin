package com.moyz.adi.common.util;

/**
 * Numeric helpers shared across services. Centralized here so common idioms (e.g. saturated long
 * → int casts for duration columns) have a single, consistent implementation.
 */
public final class NumberUtil {

    private NumberUtil() {
    }

    /**
     * Saturating cast from {@code long} to {@code int}: clamps to [0, Integer.MAX_VALUE]. Used
     * primarily for storing millisecond durations into {@code INT} columns where the upper bound
     * (~24.8 days) is well beyond any realistic duration but the input long could theoretically
     * exceed it (e.g. clock skew, artificial delays).
     *
     * @param value the long value to cast
     * @return {@code Integer.MAX_VALUE} if {@code value > Integer.MAX_VALUE}; {@code 0} if
     *         {@code value < 0}; otherwise {@code (int) value}
     */
    public static int saturatedCastToInt(long value) {
        if (value > Integer.MAX_VALUE) {
            return Integer.MAX_VALUE;
        }
        if (value < 0) {
            return 0;
        }
        return (int) value;
    }
}

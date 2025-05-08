package com.moyz.adi.common.rag;

import com.aliyun.core.utils.StringUtils;

public class RAGThreadLocal {
    private static final ThreadLocal<String> tokenEstimator = new ThreadLocal<>();

    public static void setTokenEstimator(String value) {
        tokenEstimator.set(StringUtils.isBlank(value) ? "" : value);
    }

    public static String getTokenEstimator() {
        return tokenEstimator.get();
    }

    public static void clearTokenEstimator() {
        tokenEstimator.remove();
    }
}

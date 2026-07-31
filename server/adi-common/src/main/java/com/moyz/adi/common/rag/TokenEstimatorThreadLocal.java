package com.moyz.adi.common.rag;

import com.aliyun.core.utils.StringUtils;

/**
 * Carries the tokenizer name scoped to a single user request.
 *
 * <p>A single user request may trigger multiple LLM calls (RAG retrieval, forwarding to the LLM,
 * tool-call sub-requests, etc.) scattered across many methods. Each of them needs the same tokenizer
 * to count tokens consistently. Rather than threading a {@code tokenizer} parameter through every
 * method in the call chain — which is verbose and easy to forget — the tokenizer name is set once at
 * the request entry point and read anywhere downstream via this thread-local.</p>
 *
 * <p><b>Contract:</b> the entry point must call {@link #setTokenEstimator(String)} and guarantee
 * {@link #clearTokenEstimator()} runs in a {@code finally} block so the value does not leak to the
 * next task reused on the same (e.g. thread-pool) thread. Downstream readers should tolerate a
 * missing value (returns {@code null}) and fall back to a default tokenizer.</p>
 *
 * <p><b>Limitation:</b> being a {@link ThreadLocal}, it does not propagate across async/reactive
 * boundaries (e.g. streaming LLM callback threads); readers on those threads will see no value and
 * must fall back.</p>
 */
public class TokenEstimatorThreadLocal {
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

package com.moyz.adi.common.util;

import org.apache.commons.lang3.StringUtils;

import java.util.regex.Pattern;

/**
 * 把任务失败落定的技术性 fail_reason 转成 locale 无关的用户提示编码（user_msg 列内容）。
 * 编码格式：CODE 或 CODE|参数（目前参数仅模型名），读取时经 i18n bundle 按用户 locale 渲染；
 * fail_reason 保留原始异常供排查；未识别的模式退化为 RAW|首行截断。
 */
public final class FailReasonConverter {

    /** 调用包装统一拼在末尾的模型标识（嵌入与 LLM 两条链路同格式） */
    private static final Pattern MODEL_NAME = Pattern.compile(",\\s*name:\\s*(\\S+)");

    /** 落定原因携带的失败阶段前缀，用户无需感知 */
    private static final Pattern STAGE_PREFIX = Pattern.compile("^(vectorize:|graph:|import:|qa_generate:)\\s*");

    /** i18n bundle 中的消息 key（见 i18n/messages*.properties 的 INDEX_FAIL_* 条目） */
    public static final String MODEL_CALL = "INDEX_FAIL_MODEL_CALL";
    public static final String MODEL_AUTH = "INDEX_FAIL_MODEL_AUTH";
    public static final String MODEL_RATE_LIMIT = "INDEX_FAIL_MODEL_RATE_LIMIT";
    public static final String MODEL_BAD_RESPONSE = "INDEX_FAIL_MODEL_BAD_RESPONSE";
    public static final String FILE_PARSE = "INDEX_FAIL_FILE_PARSE";

    private FailReasonConverter() {
    }

    /**
     * 产出 user_msg 列的编码串；在后台任务线程调用，绝不做 locale 相关渲染。
     */
    public static String toCode(String rawReason) {
        if (StringUtils.isBlank(rawReason)) {
            return "RAW|";
        }
        String reason = STAGE_PREFIX.matcher(rawReason.trim()).replaceFirst("");
        String lower = reason.toLowerCase();
        String model = extractModel(reason);
        if (model != null) {
            if (containsAny(lower, "unauthorized", "401", "403", "api key", "invalid key")) {
                return MODEL_AUTH + "|" + StringUtils.abbreviate(model, 400);
            }
            if (containsAny(lower, "429", "rate limit", "quota")) {
                return MODEL_RATE_LIMIT + "|" + StringUtils.abbreviate(model, 400);
            }
            if (containsAny(lower, "json", "parse")) {
                return MODEL_BAD_RESPONSE + "|" + StringUtils.abbreviate(model, 400);
            }
            return MODEL_CALL + "|" + StringUtils.abbreviate(model, 400);
        }
        if (containsAny(lower, "pdf", "parser", "unsupported format", "file format", "unsupporteddocument")) {
            return FILE_PARSE;
        }
        String firstLine = reason.split("\\n", 2)[0].trim();
        return "RAW|" + StringUtils.abbreviate(firstLine, 200);
    }

    private static String extractModel(String reason) {
        var matcher = MODEL_NAME.matcher(reason);
        return matcher.find() ? matcher.group(1) : null;
    }

    private static boolean containsAny(String lower, String... keys) {
        for (String key : keys) {
            if (lower.contains(key)) {
                return true;
            }
        }
        return false;
    }
}

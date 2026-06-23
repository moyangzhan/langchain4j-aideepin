package com.moyz.adi.common.enums;

import java.util.Arrays;
import java.util.List;

/**
 * Episodic-memory event types. The {@code code} is the lowercase string stored
 * in the database and emitted to / parsed from the LLM. Unknown LLM output is
 * normalized to {@link #GENERAL} so the column never holds an out-of-enum value.
 * <p>
 * 情景记忆事件类型。{@code code} 是存入数据库且与 LLM 交互的小写字符串。
 * LLM 返回的非法值会被归一化为 {@link #GENERAL}，保证该列不会出现枚举外的值。
 */
public enum EventType {

    TRAVEL("travel"),
    WORK("work"),
    HEALTH("health"),
    SOCIAL("social"),
    LEARNING("learning"),
    ENTERTAINMENT("entertainment"),
    FAMILY("family"),
    GENERAL("general");

    private final String code;

    EventType(String code) {
        this.code = code;
    }

    public String getCode() {
        return code;
    }

    /**
     * Parse an LLM-returned (or otherwise external) string into a known
     * {@link EventType}. Returns {@link #GENERAL} when the input is blank or
     * does not match any known code (case-insensitive).
     */
    public static EventType fromString(String value) {
        if (value == null || value.isBlank()) {
            return GENERAL;
        }
        String normalized = value.trim().toLowerCase();
        for (EventType type : values()) {
            if (type.code.equals(normalized)) {
                return type;
            }
        }
        return GENERAL;
    }

    /**
     * Comma-separated list of all codes, for embedding in LLM prompts so the
     * model picks from the allowed set.
     */
    public static String codesJoined() {
        return String.join(", ", codes());
    }

    private static List<String> codes() {
        return Arrays.stream(values()).map(EventType::getCode).toList();
    }
}

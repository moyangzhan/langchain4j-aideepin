package com.moyz.adi.common.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * 长期记忆类型枚举。同时承载两份信息：
 * <ul>
 *   <li>{@link #getValue() value}：数值编码，用于 ref 表 memory_type 列做物理路由
 *       （消息 ref 表数据量较大，整数编码比字符串更紧凑）；同时被 MyBatis-Plus
 *       通过 {@link BaseEnum}({@link com.baomidou.mybatisplus.annotation.IEnum})
 *       自动在 DB 列与枚举之间转换。</li>
 *   <li>{@link #getDesc() desc}：字符串名，用于向量库 metadata、prompt、DTO 出口。</li>
 * </ul>
 * <p>
 * 编码 append-only：分配后永不复用、永不重排；如需弃用某种记忆，把它标记为
 * deprecated 即可，编码本身保留以维持已落库数据的语义。
 * <p>
 * Long-term memory type. Each constant carries both a numeric code (for the ref
 * tables' memory_type column, auto-mapped by MyBatis-Plus via {@link BaseEnum})
 * and a canonical string name (used in vector store metadata, prompts, DTO
 * output). Codes are append-only — never reuse or reorder. To retire a type,
 * mark it deprecated but keep its code.
 */
@Getter
@AllArgsConstructor
public enum MemoryType implements BaseEnum {

    /** 语义记忆：稳定、可合并的事实型知识。 | Stable, mergeable factual knowledge. */
    SEMANTIC(1, "semantic"),

    /** 情景记忆：绑定时间线、不可合并的事件。 | Timeline-bound, non-mergeable events. */
    EPISODIC(2, "episodic"),

    /** 程序性记忆（预留，尚未启用）。 | Procedural memory, reserved for future use. */
    PROCEDURAL(3, "procedural");

    private final Integer value;
    private final String desc;

    private static final Map<Integer, MemoryType> BY_VALUE =
            Stream.of(values()).collect(Collectors.toUnmodifiableMap(MemoryType::getValue, e -> e));
    private static final Map<String, MemoryType> BY_DESC =
            Stream.of(values()).collect(Collectors.toUnmodifiableMap(MemoryType::getDesc, e -> e));

    /**
     * Resolve enum from numeric value. Throws on unknown values so the caller
     * never silently corrupts routing.
     * <p>
     * 根据数值编码反查枚举。未知编码抛 IllegalArgumentException，避免静默写错路由。
     */
    public static MemoryType fromValue(Integer value) {
        MemoryType t = value == null ? null : BY_VALUE.get(value);
        if (t == null) {
            throw new IllegalArgumentException("Unknown memory type value: " + value);
        }
        return t;
    }

    /**
     * Resolve enum from canonical string. Throws on unknown values.
     * <p>
     * 根据字符串名反查枚举。未知名抛 IllegalArgumentException。
     */
    public static MemoryType fromDesc(String desc) {
        MemoryType t = desc == null ? null : BY_DESC.get(desc);
        if (t == null) {
            throw new IllegalArgumentException("Unknown memory type desc: " + desc);
        }
        return t;
    }
}

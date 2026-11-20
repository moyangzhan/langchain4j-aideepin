package com.moyz.adi.common.util;

import java.security.SecureRandom;
import java.util.HexFormat;
import java.util.UUID;

public class UuidUtil {

    private static final SecureRandom RANDOM = new SecureRandom();

    private static long lastTs = Long.MIN_VALUE;

    private static int counter;

    private UuidUtil() {
    }

    /**
     * 生成 UUID v7（RFC 9562 单调计数器法），返回去掉横线的 32 位字符串。进程内严格单调：
     * 时间序写入让 uuid 索引右端顺序追加，同毫秒内由 12 位计数器递增；跨进程唯一性由
     * 毫秒级时间戳 + 随机计数器种子 + 62 位随机 rand_b 保障。时间戳明文内嵌、可读出创建
     * 时刻——标识符场景可接受，凭证场景请用 {@link #createToken()}。
     * <p>
     * Generate a UUID v7 (RFC 9562, monotonic counter method) as a 32-character
     * string without dashes. Strictly monotonic within the process: timestamp-
     * ordered writes keep uuid indexes appending rightward, and a 12-bit counter
     * increments within the same millisecond; cross-process uniqueness comes from
     * the millisecond timestamp, the random counter seed and the 62 random rand_b
     * bits. The embedded plaintext timestamp reveals creation time — acceptable
     * for identifiers; use {@link #createToken()} for credentials.
     */
    public static synchronized String createShort() {
        long ts = System.currentTimeMillis();
        if (ts > lastTs) {
            lastTs = ts;
            counter = RANDOM.nextInt(0x1000);
        } else if (counter < 0xFFF) {
            counter++;
        } else {
            lastTs = lastTs + 1;
            counter = RANDOM.nextInt(0x1000);
        }
        long msb = (lastTs & 0xFFFFFFFFFFFFL) << 16 | 0x7000L | counter;
        byte[] r = new byte[8];
        RANDOM.nextBytes(r);
        long randB = 0L;
        for (int i = 0; i < 8; i++) {
            randB = randB << 8 | (r[i] & 0xFFL);
        }
        long lsb = randB & 0x3FFFFFFFFFFFFFFFL | 0x8000000000000000L;
        return new UUID(msb, lsb).toString().replace("-", "");
    }

    /**
     * 生成 32 位十六进制随机令牌（128 位全随机熵、不含时间戳、无顺序性），供会话令牌、
     * 激活码、找回密码码等凭证场景使用。
     * <p>
     * Generate a 32-character hexadecimal random token (128 bits of full random
     * entropy, no timestamp, no ordering) for credentials such as session tokens,
     * activation codes and password-reset codes.
     */
    public static String createToken() {
        byte[] bytes = new byte[16];
        RANDOM.nextBytes(bytes);
        return HexFormat.of().formatHex(bytes);
    }

    /**
     * 把 32 位无横线 uuid 转成 36 位 canonical 形（pgvector 主键约束 UUID.fromString）。
     * 输入须为 32 位十六进制；传入 36 位 canonical 形时原样归一返回。
     * <p>
     * Convert a 32-char dashed-less uuid into its canonical 36-char form (required
     * by the pgvector primary key via UUID.fromString). The input must be 32 hex
     * chars; a canonical 36-char input is returned normalized as-is.
     */
    public static String toCanonical(String shortUuid) {
        String s = shortUuid.replace("-", "");
        return new StringBuilder(36)
                .append(s, 0, 8).append('-')
                .append(s, 8, 12).append('-')
                .append(s, 12, 16).append('-')
                .append(s, 16, 20).append('-')
                .append(s, 20, 32)
                .toString();
    }

    /**
     * 把 36 位 canonical uuid 转成 32 位无横线形（关系表 uuid 列、项目标识符惯例）。
     * <p>
     * Convert a canonical 36-char uuid into its 32-char dashed-less form (relational
     * uuid columns, project identifier convention).
     */
    public static String toShort(String canonicalUuid) {
        return canonicalUuid.replace("-", "");
    }
}

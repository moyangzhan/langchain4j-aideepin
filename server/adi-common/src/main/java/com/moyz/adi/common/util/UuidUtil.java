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
     * 生成 UUID v7（RFC 9562），返回去掉横线的 32 位字符串；进程内严格单调，跨进程唯一。
     * 凭证场景请用 {@link #createToken()}。
     * <p>
     * Generate a UUID v7 (RFC 9562) as a 32-character string without dashes;
     * strictly monotonic within the process, unique across processes.
     * Use {@link #createToken()} for credentials.
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
     * 生成 32 位十六进制随机令牌（128 位全随机熵、不含时间戳、无顺序性），供会话令牌、激活码、找回密码码等凭证场景使用。
     * <p>
     * Generate a 32-character hexadecimal random token (128 bits of full random entropy, no timestamp, no ordering)
     * for credentials such as session tokens, activation codes and password-reset codes.
     */
    public static String createToken() {
        byte[] bytes = new byte[16];
        RANDOM.nextBytes(bytes);
        return HexFormat.of().formatHex(bytes);
    }

    /**
     * 把 32 位无横线 uuid 转成 36 位带横线格式。
     * 输入须为 32 位十六进制；传入 36 位原样返回。
     * <p>
     * Convert a 32-char dashed-less uuid into its 36-char dashed form.
     * The input must be 32 hex chars; a 36-char input is returned as-is.
     */
    public static String toCanonical(String shortUuid) {
        String s = shortUuid.replace("-", "");
        return s.substring(0, 8) + '-' +
               s.substring(8, 12) + '-' +
               s.substring(12, 16) + '-' +
               s.substring(16, 20) + '-' +
               s.substring(20, 32);
    }

    /**
     * 把 36 位 canonical uuid 转成 32 位无横线。
     * <p>
     * Convert a canonical 36-char uuid into its 32-char dashed-less form.
     */
    public static String toShort(String canonicalUuid) {
        return canonicalUuid.replace("-", "");
    }
}

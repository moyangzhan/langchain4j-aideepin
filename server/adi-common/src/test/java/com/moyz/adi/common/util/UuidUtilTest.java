package com.moyz.adi.common.util;

import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * createShort 的 v7 契约：32 位无横线小写十六进制、版本位 7、变体位 10、时间戳域等于生成时刻
 * （毫秒级）、同批生成值单调不回退、与补横线 canonical 形态无损互转。这些是全库 73 处调用
 * 与 pgvector UUID.fromString（联接键 canonical 形）共同依赖的输出形态。
 */
class UuidUtilTest {

    @Test
    void returns32HexLowercaseV7Layout() {
        String shortUuid = UuidUtil.createShort();
        assertEquals(32, shortUuid.length());
        assertTrue(shortUuid.matches("[0-9a-f]{32}"));

        UUID parsed = UUID.fromString(shortUuid.replaceFirst("(.{8})(.{4})(.{4})(.{4})(.{12})", "$1-$2-$3-$4-$5"));
        assertEquals(7, parsed.version());
        assertEquals(2, parsed.variant());
    }

    @Test
    void timestampFieldReflectsGenerationTime() {
        long before = System.currentTimeMillis();
        String shortUuid = UuidUtil.createShort();
        long after = System.currentTimeMillis();

        long ts = Long.parseLong(shortUuid.substring(0, 12), 16);
        assertTrue(ts >= before && ts <= after);
    }

    @Test
    void sameBatchValuesAreMonotonicAndUnique() {
        List<String> values = new ArrayList<>();
        for (int i = 0; i < 1000; i++) {
            values.add(UuidUtil.createShort());
        }
        assertEquals(1000, values.stream().distinct().count());
        for (int i = 1; i < values.size(); i++) {
            assertTrue(values.get(i - 1).compareTo(values.get(i)) < 0,
                    "v7 within one millisecond must not go backwards");
        }
    }

    @Test
    void differentValuesAcrossCalls() {
        assertNotEquals(UuidUtil.createShort(), UuidUtil.createShort());
    }

    @Test
    void tokenIs32HexFullRandom() {
        String token = UuidUtil.createToken();
        assertEquals(32, token.length());
        assertTrue(token.matches("[0-9a-f]{32}"));
        assertNotEquals(token, UuidUtil.createToken());
    }

    @Test
    void canonicalAndShortFormsRoundTrip() {
        String shortUuid = UuidUtil.createShort();
        String canonical = UuidUtil.toCanonical(shortUuid);
        assertEquals(36, canonical.length());
        assertEquals('-', canonical.charAt(8));
        assertEquals(shortUuid, UuidUtil.toShort(canonical));
        assertEquals(UUID.fromString(canonical).toString(), canonical);
        assertEquals(canonical, UuidUtil.toCanonical(canonical));
    }
}

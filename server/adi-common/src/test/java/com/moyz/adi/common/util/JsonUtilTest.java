package com.moyz.adi.common.util;

import org.junit.jupiter.api.Test;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.node.ObjectNode;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * JsonUtil 的序列化契约：null 字段省略、Long 输出为字符串、LocalDateTime 固定 yyyy-MM-dd HH:mm:ss、未知属性宽容、空白输入返 null。
 * 这些行为由自定义 SimpleModule 与 mapper 配置共同决定，是前端与存量数据依赖的输出形态。
 */
class JsonUtilTest {

    record Sample(String name, Long id, LocalDateTime time, String absent) {
    }

    @Test
    void toJsonAppliesInclusionLongAsStringAndDateTimePattern() {
        JsonNode node = JsonUtil.toJsonNode(JsonUtil.toJson(new Sample("a", 42L, LocalDateTime.of(2026, 10, 30, 19, 0, 0), null)));
        assertEquals("a", node.get("name").asText());
        assertTrue(node.get("id").isString());
        assertEquals("42", node.get("id").asText());
        assertEquals("2026-10-30 19:00:00", node.get("time").asText());
        assertFalse(node.has("absent"));
    }

    @Test
    void fromJsonStringRoundTripsAndToleratesUnknownProperties() {
        Sample sample = JsonUtil.fromJson("{\"name\":\"a\",\"id\":\"42\",\"time\":\"2026-10-30 19:00:00\",\"unknown\":1}", Sample.class);
        assertEquals(new Sample("a", 42L, LocalDateTime.of(2026, 10, 30, 19, 0, 0), null), sample);
    }

    @Test
    void blankAndMalformedInputsReturnNull() {
        assertNull(JsonUtil.fromJson("", Sample.class));
        assertNull(JsonUtil.fromJson("   ", Sample.class));
        assertNull(JsonUtil.fromJson("{oops", Sample.class));
        assertNull(JsonUtil.toJsonNode(""));
        assertNull(JsonUtil.toJsonNode("   "));
        assertNull(JsonUtil.toJsonNode(null));
    }

    @Test
    void treeAndCollectionConversions() {
        assertInstanceOf(ObjectNode.class, JsonUtil.toJsonNode("{\"k\":\"v\"}"));
        List<Sample> list = JsonUtil.toList("[{\"name\":\"x\",\"id\":1},{\"name\":\"y\",\"id\":2}]", Sample.class);
        assertEquals(2, list.size());
        assertEquals(2L, list.get(1).id());
        Map<String, Object> map = JsonUtil.toMap("{\"k\":\"v\",\"n\":1}");
        assertEquals("v", map.get("k"));
        assertEquals(1, map.get("n"));
        assertEquals("a", JsonUtil.toMap(new Sample("a", 1L, null, null)).get("name"));
        assertEquals("a", JsonUtil.fromJson(JsonUtil.classToJsonNode(new Sample("a", 1L, null, null)), Sample.class).name());
    }
}

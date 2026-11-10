package com.moyz.adi.common.workflow;

import com.moyz.adi.common.util.JsonUtil;
import org.junit.jupiter.api.Test;
import tools.jackson.databind.node.ArrayNode;
import tools.jackson.databind.node.ObjectNode;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * The workflow input uuid extraction backs BOTH reference attach (run creation) and detach
 * (runtime deletion): it must pick exactly the type=4 file parameters, keep plain uuids only
 * (URLs belong to draw-node products whose references the GC recount surface covers) and
 * survive malformed shapes (missing type, non-array value, null input) without throwing.
 * <p>
 * 工作流 input 的 uuid 提取同时承重引用登记（运行建立）与解除（运行删除）：必须只取 type=4
 * 文件参数、只保留纯 uuid（URL 属画图节点产物，其引用由 GC 复核面覆盖），并对畸形结构
 * （缺 type、value 非数组、null 输入）静默防御不抛异常。
 * </p>
 */
class WfNodeIODataUtilExtractTest {

    @Test
    void extractsUuidsFromFilesParametersOnly() {
        ObjectNode input = JsonUtil.createObjectNode();
        input.set("fileParam", filesContent("uuid-a", "uuid-b"));
        input.set("textParam", textContent("uuid-not-a-file"));

        List<String> uuids = WfNodeIODataUtil.extractFileUuids(input);

        assertEquals(List.of("uuid-a", "uuid-b"), uuids);
    }

    @Test
    void filtersHttpUrlsAndDeduplicates() {
        ObjectNode input = JsonUtil.createObjectNode();
        input.set("first", filesContent("uuid-a", "https://bucket.endpoint/uuid-a.png", "http://x/y"));
        input.set("second", filesContent("uuid-a", "/file/uuid-a.png"));

        List<String> uuids = WfNodeIODataUtil.extractFileUuids(input);

        assertEquals(List.of("uuid-a"), uuids);
    }

    @Test
    void recountExtractionNormalizesUrlsToUuids() {
        ObjectNode input = JsonUtil.createObjectNode();
        input.set("first", filesContent("uuid-a", "https://bucket.endpoint/uuid-a.png", "https://x/y/uuid-b.tar.gz"));
        input.set("second", filesContent("/file/uuid-b.tar.gz", "uuid-c", "https://x/y/uuid-d."));

        List<String> uuids = WfNodeIODataUtil.extractFileUuidsForRecount(input);

        assertEquals(List.of("uuid-a", "uuid-b.tar", "uuid-c", "uuid-d."), uuids);
    }

    @Test
    void skipsNullArrayElementsInsteadOfYieldingTheLiteralStringNull() {
        ObjectNode input = JsonUtil.createObjectNode();
        input.set("first", filesContent("uuid-a", null, "uuid-b"));

        assertEquals(List.of("uuid-a", "uuid-b"), WfNodeIODataUtil.extractFileUuids(input));
        assertEquals(List.of("uuid-a", "uuid-b"), WfNodeIODataUtil.extractFileUuidsForRecount(input));
    }

    @Test
    void toleratesMalformedShapes() {
        ObjectNode input = JsonUtil.createObjectNode();
        ObjectNode missingType = JsonUtil.createObjectNode();
        missingType.putArray("value").add("uuid-x");
        input.set("noType", missingType);
        ObjectNode scalarValue = JsonUtil.createObjectNode();
        scalarValue.put("type", 4);
        scalarValue.put("value", "uuid-y");
        input.set("scalarValue", scalarValue);
        ObjectNode nullValue = JsonUtil.createObjectNode();
        nullValue.put("type", 4);
        nullValue.putNull("value");
        input.set("nullValue", nullValue);

        assertEquals(List.of(), WfNodeIODataUtil.extractFileUuids(input));
        assertEquals(List.of(), WfNodeIODataUtil.extractFileUuids(null));
    }

    private static ObjectNode filesContent(String... values) {
        ObjectNode content = JsonUtil.createObjectNode();
        content.put("type", 4);
        ArrayNode array = content.putArray("value");
        for (String value : values) {
            array.add(value);
        }
        return content;
    }

    private static ObjectNode textContent(String value) {
        ObjectNode content = JsonUtil.createObjectNode();
        content.put("type", 1);
        content.put("value", value);
        return content;
    }
}

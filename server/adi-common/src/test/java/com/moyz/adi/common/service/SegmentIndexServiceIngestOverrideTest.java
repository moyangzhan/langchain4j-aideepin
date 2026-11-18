package com.moyz.adi.common.service;

import com.moyz.adi.common.entity.KbDocument;
import com.moyz.adi.common.entity.KnowledgeBase;
import com.moyz.adi.common.rag.CustomSeparatorSplitter;
import dev.langchain4j.data.document.DocumentSplitter;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;

/**
 * 文档级分段参数覆盖的取效合并：四键同名回退（'' / -1 = 未覆盖），全部经 createSplitter 单点合并。
 * 该合并是通用切分、父子切分与 QA 生成切原文的公共入口。
 * <p>
 * Pins the document-level segmentation override merge: same-name four-key fallback
 * ('' / -1 = unset) through the single createSplitter funnel shared by text, parent-child
 * and QA-generation splitting.
 * </p>
 */
class SegmentIndexServiceIngestOverrideTest {

    private final SegmentIndexService service = new SegmentIndexService();

    @Test
    void effectiveMaxSegmentSizePrefersDocumentOverride() {
        KnowledgeBase kb = kb("recursive", 30, "");
        assertEquals(600, service.effectiveMaxSegmentSize(kb, doc("line", 600, 50, "#")));
        assertEquals(0, service.effectiveMaxSegmentSize(kb, doc("line", 0, 50, "#")));
    }

    @Test
    void effectiveMaxSegmentSizeFallsBackToKbWhenUnset() {
        KnowledgeBase kb = kb("recursive", 30, "");
        kb.setIngestMaxSegmentSize(1000);
        assertEquals(1000, service.effectiveMaxSegmentSize(kb, doc("", -1, -1, "")));
    }

    @Test
    void createSplitterMergesDocumentOverrides() throws Exception {
        KnowledgeBase kb = kb("recursive", 30, "");
        KbDocument doc = doc("custom", -1, 50, "###");
        DocumentSplitter splitter = service.createSplitter(kb, doc, 1000);
        CustomSeparatorSplitter custom = assertInstanceOf(CustomSeparatorSplitter.class, splitter);
        assertEquals("###", field(custom, "separator"));
        assertEquals(50, field(custom, "maxOverlapSize"));
    }

    @Test
    void createSplitterFallsBackToKbWhenUnset() throws Exception {
        KnowledgeBase kb = kb("custom", 30, "\n\n");
        KbDocument doc = doc("", -1, -1, "");
        DocumentSplitter splitter = service.createSplitter(kb, doc, 1000);
        CustomSeparatorSplitter custom = assertInstanceOf(CustomSeparatorSplitter.class, splitter);
        assertEquals("\n\n", field(custom, "separator"));
        assertEquals(30, field(custom, "maxOverlapSize"));
    }

    private static KnowledgeBase kb(String strategy, Integer overlap, String separator) {
        KnowledgeBase kb = new KnowledgeBase();
        kb.setIngestSplitStrategy(strategy);
        kb.setIngestMaxSegmentSize(1000);
        kb.setIngestMaxOverlapSize(overlap);
        kb.setIngestCustomSeparator(separator);
        kb.setIngestTokenEstimator("");
        return kb;
    }

    private static KbDocument doc(String strategy, Integer size, Integer overlap, String separator) {
        KbDocument doc = new KbDocument();
        doc.setIngestSplitStrategy(strategy);
        doc.setIngestMaxSegmentSize(size);
        doc.setIngestMaxOverlapSize(overlap);
        doc.setIngestCustomSeparator(separator);
        return doc;
    }

    private static Object field(Object target, String name) throws Exception {
        for (Class<?> c = target.getClass(); c != null; c = c.getSuperclass()) {
            try {
                Field f = c.getDeclaredField(name);
                f.setAccessible(true);
                return f.get(target);
            } catch (NoSuchFieldException ignored) {
                // walk up the hierarchy
            }
        }
        throw new IllegalStateException("No " + name + " field on " + target.getClass().getName());
    }
}

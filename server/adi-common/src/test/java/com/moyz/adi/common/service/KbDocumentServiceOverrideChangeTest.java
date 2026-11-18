package com.moyz.adi.common.service;

import com.moyz.adi.common.dto.KbDocumentEditReq;
import com.moyz.adi.common.entity.KbDocument;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * 覆盖变更判定驱动「变更即失效」：任一已传键不同即为变更；未传(null)键不参与比较（部分更新语义）。
 * This pins the override-change predicate behind invalidation-on-change: any provided key that
 * differs counts as a change; keys absent (null) from the request are skipped (partial update).
 */
class KbDocumentServiceOverrideChangeTest {

    private final KbDocumentService service = new KbDocumentService();

    @Test
    void anyProvidedKeyDifferenceCountsAsChange() {
        assertTrue(service.segmentOverrideChanged(doc("line", -1, -1, ""), req("line", -1, -1, "###")));
        assertTrue(service.segmentOverrideChanged(doc("line", 500, -1, ""), req("line", -1, -1, "")));
        assertTrue(service.segmentOverrideChanged(doc("", -1, -1, ""), req("", -1, 80, "")));
        assertTrue(service.segmentOverrideChanged(doc("", -1, -1, ""), req("line", -1, -1, "")));
    }

    @Test
    void clearingAnOverrideCountsAsChange() {
        KbDocument old = doc("line", 500, 80, "###");
        assertTrue(service.segmentOverrideChanged(old, req("", -1, -1, "")));
    }

    @Test
    void absentKeysDoNotTriggerChange() {
        assertFalse(service.segmentOverrideChanged(doc("line", 500, 80, "###"), req(null, null, null, null)));
        assertFalse(service.segmentOverrideChanged(doc("", -1, -1, ""), req(null, null, null, null)));
    }

    @Test
    void identicalValuesDoNotTriggerChange() {
        assertFalse(service.segmentOverrideChanged(doc("line", 500, 80, "###"), req("line", 500, 80, "###")));
        assertFalse(service.segmentOverrideChanged(doc("", -1, -1, ""), req("", -1, -1, "")));
    }

    private static KbDocument doc(String strategy, Integer size, Integer overlap, String separator) {
        KbDocument doc = new KbDocument();
        doc.setIngestSplitStrategy(strategy);
        doc.setIngestMaxSegmentSize(size);
        doc.setIngestMaxOverlapSize(overlap);
        doc.setIngestCustomSeparator(separator);
        return doc;
    }

    private static KbDocumentEditReq req(String strategy, Integer size, Integer overlap, String separator) {
        KbDocumentEditReq req = new KbDocumentEditReq();
        req.setIngestSplitStrategy(strategy);
        req.setIngestMaxSegmentSize(size);
        req.setIngestMaxOverlapSize(overlap);
        req.setIngestCustomSeparator(separator);
        return req;
    }
}

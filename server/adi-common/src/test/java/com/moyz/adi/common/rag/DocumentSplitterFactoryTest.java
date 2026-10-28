package com.moyz.adi.common.rag;

import dev.langchain4j.data.document.DocumentSplitter;
import dev.langchain4j.model.TokenCountEstimator;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;

import static com.moyz.adi.common.cosntant.AdiConstant.SplitStrategy;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Overlap normalization at the splitter factory entry.
 * The effective overlap must stay within [0, maxSegmentSize / 2] so langchain4j splitters never receive a degenerate overlap >= maxSegmentSize (they would emit segments made mostly of repeated prefix text).
 */
class DocumentSplitterFactoryTest {

    @Test
    void negativeOverlapClampsToZero() {
        assertEquals(0, DocumentSplitterFactory.normalizeOverlap(-5, 1000));
    }

    @Test
    void negativeSizeStaysNonNegative() {
        assertEquals(0, DocumentSplitterFactory.normalizeOverlap(-5, -10));
        assertEquals(0, DocumentSplitterFactory.normalizeOverlap(3, -10));
        assertEquals(0, DocumentSplitterFactory.normalizeOverlap(3, 0));
    }

    @Test
    void overlapWithinHalfPassesThroughUnchanged() {
        assertEquals(200, DocumentSplitterFactory.normalizeOverlap(200, 1000));
        assertEquals(0, DocumentSplitterFactory.normalizeOverlap(0, 1000));
    }

    @Test
    void overlapAtOrAboveHalfTheSizeClampsToHalf() {
        assertEquals(500, DocumentSplitterFactory.normalizeOverlap(500, 1000));
        assertEquals(500, DocumentSplitterFactory.normalizeOverlap(800, 1000));
        assertEquals(500, DocumentSplitterFactory.normalizeOverlap(1000, 1000));
    }

    @Test
    void oddSizeUsesIntegerDivision() {
        assertEquals(7, DocumentSplitterFactory.normalizeOverlap(7, 15));
        assertEquals(7, DocumentSplitterFactory.normalizeOverlap(8, 15));
    }

    /**
     * create() must hand the normalized overlap to every splitter branch: a regression re-passing the raw overlap in one strategy would otherwise keep passing the normalizeOverlap tests.
     * The splitters expose no getters, so the protected maxOverlapSize field is read reflectively.
     */
    @Test
    void createWiresNormalizedOverlapIntoEverySplitter() throws Exception {
        TokenCountEstimator estimator = charCountEstimator();
        String[] strategies = {
                SplitStrategy.RECURSIVE, SplitStrategy.PARAGRAPH, SplitStrategy.LINE,
                SplitStrategy.SENTENCE, SplitStrategy.CUSTOM, "unknown-falls-to-default"};
        for (String strategy : strategies) {
            DocumentSplitter splitter = DocumentSplitterFactory.create(strategy, 100, 80, "\n\n", estimator);
            assertEquals(50, maxOverlapSize(splitter), "strategy " + strategy);
        }
    }

    /**
     * Character-count estimator; only estimateTokenCountInText is on the splitting path — the two message methods just satisfy the (non-functional) interface.
     */
    private static TokenCountEstimator charCountEstimator() {
        return new TokenCountEstimator() {
            @Override
            public int estimateTokenCountInText(String text) {
                return text == null ? 0 : text.length();
            }

            @Override
            public int estimateTokenCountInMessage(dev.langchain4j.data.message.ChatMessage message) {
                return 0;
            }

            @Override
            public int estimateTokenCountInMessages(Iterable<dev.langchain4j.data.message.ChatMessage> messages) {
                return 0;
            }
        };
    }

    private static int maxOverlapSize(DocumentSplitter splitter) throws Exception {
        for (Class<?> c = splitter.getClass(); c != null; c = c.getSuperclass()) {
            try {
                Field field = c.getDeclaredField("maxOverlapSize");
                field.setAccessible(true);
                return field.getInt(splitter);
            } catch (NoSuchFieldException ignored) {
                // walk up the hierarchy
            }
        }
        throw new IllegalStateException("No maxOverlapSize field on " + splitter.getClass().getName());
    }
}

package com.moyz.adi.common.rag;

import dev.langchain4j.data.document.DocumentSplitter;
import dev.langchain4j.data.document.splitter.DocumentByLineSplitter;
import dev.langchain4j.data.document.splitter.DocumentByParagraphSplitter;
import dev.langchain4j.data.document.splitter.DocumentBySentenceSplitter;
import dev.langchain4j.data.document.splitter.DocumentSplitters;
import dev.langchain4j.model.TokenCountEstimator;
import lombok.extern.slf4j.Slf4j;

import static com.moyz.adi.common.cosntant.AdiConstant.SplitStrategy.*;

/**
 * Single entry for building ingest-side document splitters.
 * langchain4j 1.14.1 splitters accept overlap &gt;= maxSegmentSize without validation and then emit degenerate segments made mostly of the repeated overlap prefix,
 * so overlap is clamped here to [0, maxSegmentSize / 2]; clamping only warns and never blocks indexing.
 * The clamp is relative to the size of the current call: KB-level segment size for text/parent/QA generation, document-level child chunk size for child splitters.
 */
@Slf4j
public class DocumentSplitterFactory {

    public static DocumentSplitter create(String strategy, int maxSegmentSize,
                                          int overlap, String customSeparator,
                                          TokenCountEstimator estimator) {
        int effectiveOverlap = normalizeOverlap(overlap, maxSegmentSize);
        return switch (strategy) {
            case PARAGRAPH -> new DocumentByParagraphSplitter(maxSegmentSize, effectiveOverlap, estimator);
            case LINE -> new DocumentByLineSplitter(maxSegmentSize, effectiveOverlap, estimator);
            case SENTENCE -> new DocumentBySentenceSplitter(maxSegmentSize, effectiveOverlap, estimator);
            case CUSTOM -> new CustomSeparatorSplitter(maxSegmentSize, effectiveOverlap, customSeparator, estimator);
            default -> DocumentSplitters.recursive(maxSegmentSize, effectiveOverlap, estimator);
        };
    }

    static int normalizeOverlap(int overlap, int maxSegmentSize) {
        // the max(…, 0) on the size half keeps the result non-negative even for a bogus negative size
        int effective = Math.min(Math.max(overlap, 0), Math.max(maxSegmentSize / 2, 0));
        if (effective != overlap) {
            log.warn("Splitter overlap {} out of safe range for maxSegmentSize {}, clamped to {}",
                    overlap, maxSegmentSize, effective);
        }
        return effective;
    }
}

package com.moyz.adi.common.rag;

import dev.langchain4j.data.document.DocumentSplitter;
import dev.langchain4j.data.document.splitter.DocumentByLineSplitter;
import dev.langchain4j.data.document.splitter.DocumentByParagraphSplitter;
import dev.langchain4j.data.document.splitter.DocumentBySentenceSplitter;
import dev.langchain4j.data.document.splitter.DocumentSplitters;
import dev.langchain4j.model.TokenCountEstimator;

import static com.moyz.adi.common.cosntant.AdiConstant.SplitStrategy.*;

public class DocumentSplitterFactory {

    public static DocumentSplitter create(String strategy, int maxSegmentSize,
                                          int overlap, String customSeparator,
                                          TokenCountEstimator estimator) {
        return switch (strategy) {
            case PARAGRAPH -> new DocumentByParagraphSplitter(maxSegmentSize, overlap, estimator);
            case LINE -> new DocumentByLineSplitter(maxSegmentSize, overlap, estimator);
            case SENTENCE -> new DocumentBySentenceSplitter(maxSegmentSize, overlap, estimator);
            case CUSTOM -> new CustomSeparatorSplitter(maxSegmentSize, overlap, customSeparator, estimator);
            default -> DocumentSplitters.recursive(maxSegmentSize, overlap, estimator);
        };
    }
}

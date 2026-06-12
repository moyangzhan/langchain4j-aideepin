package com.moyz.adi.common.rag;

import dev.langchain4j.data.document.DocumentSplitter;
import dev.langchain4j.data.document.splitter.DocumentBySentenceSplitter;
import dev.langchain4j.data.document.splitter.HierarchicalDocumentSplitter;
import dev.langchain4j.model.TokenCountEstimator;

import java.util.regex.Pattern;

/**
 * Splits text by a user-defined separator string, then assembles segments
 * respecting the max segment size (in tokens or characters).
 */
public class CustomSeparatorSplitter extends HierarchicalDocumentSplitter {

    private final String separator;
    private final Pattern splitPattern;
    private final int maxSegmentSize;
    private final int maxOverlapSize;
    private final TokenCountEstimator estimator;

    public CustomSeparatorSplitter(int maxSegmentSize,
                                   int maxOverlapSize,
                                   String separator,
                                   TokenCountEstimator estimator) {
        super(maxSegmentSize, maxOverlapSize, estimator, new DocumentBySentenceSplitter(maxSegmentSize, maxOverlapSize, estimator));
        this.maxSegmentSize = maxSegmentSize;
        this.maxOverlapSize = maxOverlapSize;
        this.estimator = estimator;
        this.separator = separator;
        this.splitPattern = Pattern.compile(Pattern.quote(separator));
    }

    @Override
    public String[] split(String text) {
        return splitPattern.split(text);
    }

    @Override
    public String joinDelimiter() {
        return separator;
    }

    @Override
    protected DocumentSplitter defaultSubSplitter() {
        return new DocumentBySentenceSplitter(maxSegmentSize, maxOverlapSize, estimator);
    }
}

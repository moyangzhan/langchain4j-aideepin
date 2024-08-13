package com.moyz.adi.common.util;

import com.moyz.adi.common.exception.BaseException;
import dev.langchain4j.data.embedding.Embedding;
import dev.langchain4j.data.segment.TextSegment;
import dev.langchain4j.model.embedding.EmbeddingModel;
import dev.langchain4j.rag.content.Content;
import dev.langchain4j.rag.content.retriever.ContentRetriever;
import dev.langchain4j.rag.query.Query;
import dev.langchain4j.spi.model.embedding.EmbeddingModelFactory;
import dev.langchain4j.store.embedding.EmbeddingMatch;
import dev.langchain4j.store.embedding.EmbeddingSearchRequest;
import dev.langchain4j.store.embedding.EmbeddingSearchResult;
import dev.langchain4j.store.embedding.EmbeddingStore;
import dev.langchain4j.store.embedding.filter.Filter;
import lombok.Builder;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;

import java.util.*;
import java.util.function.Function;

import static com.moyz.adi.common.enums.ErrorEnum.B_BREAK_SEARCH;
import static dev.langchain4j.internal.Utils.getOrDefault;
import static dev.langchain4j.internal.ValidationUtils.*;
import static dev.langchain4j.spi.ServiceHelper.loadFactories;
import static java.util.stream.Collectors.toList;

/**
 * 复制dev.langchain4j.rag.content.retriever.EmbeddingStoreContentRetriever并做了少许改动；
 * 增加支持：缓存命中的向量以便后续记录到数据库中
 */
@Slf4j
public class AdiEmbeddingStoreContentRetriever implements ContentRetriever {
    public static final Function<Query, Integer> DEFAULT_MAX_RESULTS = (query) -> 3;
    public static final Function<Query, Double> DEFAULT_MIN_SCORE = (query) -> 0.0;
    public static final Function<Query, Filter> DEFAULT_FILTER = (query) -> null;

    public static final String DEFAULT_DISPLAY_NAME = "Default";

    private final EmbeddingStore<TextSegment> embeddingStore;
    private final EmbeddingModel embeddingModel;

    private final Function<Query, Integer> maxResultsProvider;
    private final Function<Query, Double> minScoreProvider;
    private final Function<Query, Filter> filterProvider;

    private final String displayName;

    /**
     * 新增的特性: 命中的向量及对应的分数
     */
    private Map<String, Double> embeddingToScore = new HashMap<>();

    private boolean breakIfSearchMissed = false;

    public AdiEmbeddingStoreContentRetriever(EmbeddingStore<TextSegment> embeddingStore,
                                             EmbeddingModel embeddingModel) {
        this(
                DEFAULT_DISPLAY_NAME,
                embeddingStore,
                embeddingModel,
                DEFAULT_MAX_RESULTS,
                DEFAULT_MIN_SCORE,
                DEFAULT_FILTER,
                false
        );
    }

    public AdiEmbeddingStoreContentRetriever(EmbeddingStore<TextSegment> embeddingStore,
                                             EmbeddingModel embeddingModel,
                                             int maxResults) {
        this(
                DEFAULT_DISPLAY_NAME,
                embeddingStore,
                embeddingModel,
                (query) -> maxResults,
                DEFAULT_MIN_SCORE,
                DEFAULT_FILTER,
                false
        );
    }

    public AdiEmbeddingStoreContentRetriever(EmbeddingStore<TextSegment> embeddingStore,
                                             EmbeddingModel embeddingModel,
                                             Integer maxResults,
                                             Double minScore) {
        this(
                DEFAULT_DISPLAY_NAME,
                embeddingStore,
                embeddingModel,
                (query) -> maxResults,
                (query) -> minScore,
                DEFAULT_FILTER,
                false
        );
    }

    @Builder
    private AdiEmbeddingStoreContentRetriever(String displayName,
                                              EmbeddingStore<TextSegment> embeddingStore,
                                              EmbeddingModel embeddingModel,
                                              Function<Query, Integer> dynamicMaxResults,
                                              Function<Query, Double> dynamicMinScore,
                                              Function<Query, Filter> dynamicFilter,
                                              Boolean breakIfSearchMissed) {
        this.displayName = getOrDefault(displayName, DEFAULT_DISPLAY_NAME);
        this.embeddingStore = ensureNotNull(embeddingStore, "embeddingStore");
        this.embeddingModel = ensureNotNull(
                getOrDefault(embeddingModel, AdiEmbeddingStoreContentRetriever::loadEmbeddingModel),
                "embeddingModel"
        );
        this.maxResultsProvider = getOrDefault(dynamicMaxResults, DEFAULT_MAX_RESULTS);
        this.minScoreProvider = getOrDefault(dynamicMinScore, DEFAULT_MIN_SCORE);
        this.filterProvider = getOrDefault(dynamicFilter, DEFAULT_FILTER);
        this.breakIfSearchMissed = breakIfSearchMissed;
    }

    private static EmbeddingModel loadEmbeddingModel() {
        Collection<EmbeddingModelFactory> factories = loadFactories(EmbeddingModelFactory.class);
        if (factories.size() > 1) {
            throw new RuntimeException("Conflict: multiple embedding models have been found in the classpath. " +
                    "Please explicitly specify the one you wish to use.");
        }

        for (EmbeddingModelFactory factory : factories) {
            return factory.create();
        }

        return null;
    }

    public static class AdiEmbeddingStoreContentRetrieverBuilder {

        public AdiEmbeddingStoreContentRetrieverBuilder maxResults(Integer maxResults) {
            if (maxResults != null) {
                dynamicMaxResults = (query) -> ensureGreaterThanZero(maxResults, "maxResults");
            }
            return this;
        }

        public AdiEmbeddingStoreContentRetrieverBuilder minScore(Double minScore) {
            if (minScore != null) {
                dynamicMinScore = (query) -> ensureBetween(minScore, 0, 1, "minScore");
            }
            return this;
        }

        public AdiEmbeddingStoreContentRetrieverBuilder filter(Filter filter) {
            if (filter != null) {
                dynamicFilter = (query) -> filter;
            }
            return this;
        }

        public AdiEmbeddingStoreContentRetrieverBuilder breakIfSearchMissed(boolean breakFlag) {
            breakIfSearchMissed = breakFlag;
            return this;
        }

    }

    /**
     * Creates an instance of an {@code EmbeddingStoreContentRetriever} from the specified {@link EmbeddingStore}
     * and {@link EmbeddingModel} found through SPI (see {@link EmbeddingModelFactory}).
     */
    public static AdiEmbeddingStoreContentRetriever from(EmbeddingStore<TextSegment> embeddingStore) {
        return builder().embeddingStore(embeddingStore).build();
    }

    @Override
    public List<Content> retrieve(Query query) {

        Embedding embeddedQuery = embeddingModel.embed(query.text()).content();

        EmbeddingSearchRequest searchRequest = EmbeddingSearchRequest.builder()
                .queryEmbedding(embeddedQuery)
                .maxResults(maxResultsProvider.apply(query))
                .minScore(minScoreProvider.apply(query))
                .filter(filterProvider.apply(query))
                .build();

        EmbeddingSearchResult<TextSegment> searchResult = embeddingStore.search(searchRequest);

        List<Content> result = searchResult.matches().stream()
                .peek(item -> {
                    embeddingToScore.put(item.embeddingId(), item.score());
                    log.info("embeddingToScore,embeddingId:{},score:{}", item.embeddingId(), item.score());
                })
                .map(EmbeddingMatch::embedded)
                .map(Content::from)
                .collect(toList());

        //判断是否要强行中断查询，没有命中则不再请求LLM，直接抛出异常中断流程
        if (breakIfSearchMissed && CollectionUtils.isEmpty(result)) {
            throw new BaseException(B_BREAK_SEARCH);
        }
        return result;
    }

    /**
     * aideepin新增方法
     *
     * @return
     */
    public Map<String, Double> getRetrievedEmbeddingToScore() {
        return this.embeddingToScore;
    }

    @Override
    public String toString() {
        return "AdiEmbeddingStoreContentRetriever{" +
                "displayName='" + displayName + '\'' +
                '}';
    }
}

package com.moyz.adi.common.rag;

import com.moyz.adi.common.util.InputAdaptor;
import com.moyz.adi.common.vo.InputAdaptorMsg;
import dev.langchain4j.data.message.ChatMessage;
import dev.langchain4j.data.message.UserMessage;
import dev.langchain4j.rag.AugmentationRequest;
import dev.langchain4j.rag.AugmentationResult;
import dev.langchain4j.rag.RetrievalAugmentor;
import dev.langchain4j.rag.content.Content;
import dev.langchain4j.rag.content.aggregator.ContentAggregator;
import dev.langchain4j.rag.content.aggregator.DefaultContentAggregator;
import dev.langchain4j.rag.content.injector.ContentInjector;
import dev.langchain4j.rag.content.injector.DefaultContentInjector;
import dev.langchain4j.rag.content.retriever.ContentRetriever;
import dev.langchain4j.rag.query.Metadata;
import dev.langchain4j.rag.query.Query;
import dev.langchain4j.rag.query.router.DefaultQueryRouter;
import dev.langchain4j.rag.query.router.QueryRouter;
import dev.langchain4j.rag.query.transformer.DefaultQueryTransformer;
import dev.langchain4j.rag.query.transformer.QueryTransformer;
import lombok.Builder;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;

import static dev.langchain4j.internal.Utils.getOrDefault;
import static dev.langchain4j.internal.ValidationUtils.ensureNotNull;
import static java.util.Collections.*;
import static java.util.Collections.emptyMap;
import static java.util.concurrent.CompletableFuture.allOf;
import static java.util.concurrent.CompletableFuture.supplyAsync;
import static java.util.concurrent.TimeUnit.SECONDS;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toMap;

/**
 * 知识库召回增强器<br/>
 * 复制dev.langchain4j.rag.DefaultRetrievalAugmentor并进行少许改造
 */
public class AdiKnowledgeBaseRetrievalAugmentor implements RetrievalAugmentor {

    private static final Logger log = LoggerFactory.getLogger(AdiKnowledgeBaseRetrievalAugmentor.class);

    private final QueryTransformer queryTransformer;
    private final QueryRouter queryRouter;
    private final ContentAggregator contentAggregator;
    private final ContentInjector contentInjector;
    private final Executor executor;

    private Consumer<InputAdaptorMsg> inputAdaptorMsgConsumer;
    private int maxInputTokens;

    @Builder
    public AdiKnowledgeBaseRetrievalAugmentor(QueryTransformer queryTransformer,
                                              QueryRouter queryRouter,
                                              ContentAggregator contentAggregator,
                                              ContentInjector contentInjector,
                                              Executor executor,

                                              Consumer<InputAdaptorMsg> inputAdaptorMsgConsumer,
                                              int maxInputTokens) {
        this.queryTransformer = getOrDefault(queryTransformer, DefaultQueryTransformer::new);
        this.queryRouter = ensureNotNull(queryRouter, "queryRouter");
        this.contentAggregator = getOrDefault(contentAggregator, DefaultContentAggregator::new);
        this.contentInjector = getOrDefault(contentInjector, DefaultContentInjector::new);
        this.executor = getOrDefault(executor, AdiKnowledgeBaseRetrievalAugmentor::createDefaultExecutor);

        this.inputAdaptorMsgConsumer = inputAdaptorMsgConsumer;
        this.maxInputTokens = maxInputTokens;
    }

    private static ExecutorService createDefaultExecutor() {
        return new ThreadPoolExecutor(
                0, Integer.MAX_VALUE,
                1, SECONDS,
                new SynchronousQueue<>()
        );
    }

    /**
     * @deprecated use {@link #augment(AugmentationRequest)} instead.
     */
    @Override
    @Deprecated
    public UserMessage augment(UserMessage userMessage, Metadata metadata) {
        AugmentationRequest augmentationRequest = new AugmentationRequest(userMessage, metadata);
        return (UserMessage) augment(augmentationRequest).chatMessage();
    }

    @Override
    public AugmentationResult augment(AugmentationRequest augmentationRequest) {

        ChatMessage chatMessage = augmentationRequest.chatMessage();
        Metadata metadata = augmentationRequest.metadata();

        //=======aideepin --begin
        AtomicInteger questionTokenCount = new AtomicInteger();
        Metadata validMetadata = InputAdaptor.adjustMemory(augmentationRequest, maxInputTokens, (inputAdaptorMsg -> {
            inputAdaptorMsgConsumer.accept(inputAdaptorMsg);
            questionTokenCount.set(inputAdaptorMsg.getUserQuestionTokenCount());
        }));
        //=======aideepin --end

        Query originalQuery = Query.from(chatMessage.text(), validMetadata);

        Collection<Query> queries = queryTransformer.transform(originalQuery);
        logQueries(originalQuery, queries);

        Map<Query, Collection<List<Content>>> queryToContents = process(queries);

        List<Content> contents = contentAggregator.aggregate(queryToContents);
        log(queryToContents, contents);

        //=======aideepin --begin
        List<Content> validContents = InputAdaptor.adjustRetrieveDocs(questionTokenCount.get(), contents, maxInputTokens);
        //=======aideepin --end

        ChatMessage augmentedChatMessage = contentInjector.inject(validContents, chatMessage);
        log(augmentedChatMessage);

        return AugmentationResult.builder()
                .chatMessage(augmentedChatMessage)
                .contents(validContents)
                .build();
    }

    private Map<Query, Collection<List<Content>>> process(Collection<Query> queries) {
        if (queries.size() == 1) {
            Query query = queries.iterator().next();
            Collection<ContentRetriever> retrievers = queryRouter.route(query);
            if (retrievers.size() == 1) {
                ContentRetriever contentRetriever = retrievers.iterator().next();
                List<Content> contents = contentRetriever.retrieve(query);
                return singletonMap(query, singletonList(contents));
            } else if (retrievers.size() > 1) {
                Collection<List<Content>> contents = retrieveFromAll(retrievers, query).join();
                return singletonMap(query, contents);
            } else {
                return emptyMap();
            }
        } else if (queries.size() > 1) {
            Map<Query, CompletableFuture<Collection<List<Content>>>> queryToFutureContents = new ConcurrentHashMap<>();
            queries.forEach(query -> {
                CompletableFuture<Collection<List<Content>>> futureContents =
                        supplyAsync(() -> {
                                    Collection<ContentRetriever> retrievers = queryRouter.route(query);
                                    log(query, retrievers);
                                    return retrievers;
                                },
                                executor
                        ).thenCompose(retrievers -> retrieveFromAll(retrievers, query));
                queryToFutureContents.put(query, futureContents);
            });
            return join(queryToFutureContents);
        } else {
            return emptyMap();
        }
    }

    private CompletableFuture<Collection<List<Content>>> retrieveFromAll(Collection<ContentRetriever> retrievers,
                                                                         Query query) {
        List<CompletableFuture<List<Content>>> futureContents = retrievers.stream()
                .map(retriever -> supplyAsync(() -> retrieve(retriever, query), executor))
                .toList();

        return allOf(futureContents.toArray(new CompletableFuture[0]))
                .thenApply(ignored ->
                        futureContents.stream()
                                .map(CompletableFuture::join)
                                .toList());
    }

    private static List<Content> retrieve(ContentRetriever retriever, Query query) {
        List<Content> contents = retriever.retrieve(query);
        log(query, retriever, contents);
        return contents;
    }

    private static Map<Query, Collection<List<Content>>> join(
            Map<Query, CompletableFuture<Collection<List<Content>>>> queryToFutureContents) {
        return allOf(queryToFutureContents.values().toArray(new CompletableFuture[0]))
                .thenApply(ignored ->
                        queryToFutureContents.entrySet().stream()
                                .collect(toMap(
                                        Map.Entry::getKey,
                                        entry -> entry.getValue().join()
                                ))
                ).join();
    }

    private static void logQueries(Query originalQuery, Collection<Query> queries) {
        if (queries.size() == 1) {
            Query transformedQuery = queries.iterator().next();
            if (!transformedQuery.equals(originalQuery)) {
                log.debug("Transformed original query '{}' into '{}'",
                        originalQuery.text(), transformedQuery.text());
            }
        } else if (log.isDebugEnabled()){
            log.debug("Transformed original query '{}' into the following queries:\n{}",
                    originalQuery.text(), queries.stream()
                            .map(Query::text)
                            .map(query -> "- '" + query + "'")
                            .collect(joining("\n")));
        }
    }

    private static void log(Query query, Collection<ContentRetriever> retrievers) {
        // TODO use retriever id
        if (retrievers.size() == 1) {
            log.debug("Routing query '{}' to the following retriever: {}",
                    query.text(), retrievers.iterator().next());
        } else if (log.isDebugEnabled()) {
            log.debug("Routing query '{}' to the following retrievers:\n{}",
                    query.text(), retrievers.stream()
                            .map(retriever -> "- " + retriever.toString())
                            .collect(joining("\n")));
        }
    }

    private static void log(Query query, ContentRetriever retriever, List<Content> contents) {
        // TODO use retriever id
        log.debug("Retrieved {} contents using query '{}' and retriever '{}'",
                contents.size(), query.text(), retriever);

        if (!log.isTraceEnabled()) {
            return;
        }

        if (!contents.isEmpty()) {
            final var contentsSting = contents.stream()
                    .map(Content::textSegment)
                    .map(segment -> "- " + escapeNewlines(segment.text()))
                    .collect(joining("\n"));
            log.trace("Retrieved {} contents using query '{}' and retriever '{}':\n{}",
                    contents.size(),
                    query.text(),
                    retriever.getClass().getName(),
                    contentsSting);
        } else {
            log.trace("Retrieved 0 contents using query '{}' and retriever '{}'",
                    query.text(),
                    retriever.getClass().getName());
        }

    }

    private static void log(Map<Query, Collection<List<Content>>> queryToContents, List<Content> contents) {

        int contentCount = 0;
        for (Map.Entry<Query, Collection<List<Content>>> entry : queryToContents.entrySet()) {
            for (List<Content> contentList : entry.getValue()) {
                contentCount += contentList.size();
            }
        }
        if (contentCount == contents.size()) {
            return;
        }

        log.debug("Aggregated {} content(s) into {}", contentCount, contents.size());

        if (log.isTraceEnabled()) {
            log.trace("Aggregated {} content(s) into:\n{}",
                    contentCount, contents.stream()
                            .map(Content::textSegment)
                            .map(segment -> "- " + escapeNewlines(segment.text()))
                            .collect(joining("\n")));
        }
    }

    private static void log(ChatMessage augmentedChatMessage) {
        if (log.isTraceEnabled()) {
            log.trace("Augmented chat message: {}",
                    escapeNewlines(augmentedChatMessage.text())
            );
        }
    }

    private static String escapeNewlines(String text) {
        return text.replace("\n", "\\n");
    }

    public static AdiKnowledgeBaseRetrievalAugmentor.AdiKnowledgeBaseRetrievalAugmentorBuilder builder() {
        return new AdiKnowledgeBaseRetrievalAugmentor.AdiKnowledgeBaseRetrievalAugmentorBuilder();
    }

    public static class AdiKnowledgeBaseRetrievalAugmentorBuilder {

        private QueryTransformer queryTransformer;
        private QueryRouter queryRouter;
        private ContentAggregator contentAggregator;
        private ContentInjector contentInjector;
        private Executor executor;

        private Consumer<InputAdaptorMsg> inputAdaptorMsgConsumer;
        private int maxInputTokens;

        AdiKnowledgeBaseRetrievalAugmentorBuilder() {
        }

        public AdiKnowledgeBaseRetrievalAugmentor.AdiKnowledgeBaseRetrievalAugmentorBuilder contentRetriever(ContentRetriever contentRetriever) {
            this.queryRouter = new DefaultQueryRouter(ensureNotNull(contentRetriever, "contentRetriever"));
            return this;
        }

        public AdiKnowledgeBaseRetrievalAugmentor.AdiKnowledgeBaseRetrievalAugmentorBuilder queryTransformer(QueryTransformer queryTransformer) {
            this.queryTransformer = queryTransformer;
            return this;
        }

        public AdiKnowledgeBaseRetrievalAugmentor.AdiKnowledgeBaseRetrievalAugmentorBuilder queryRouter(QueryRouter queryRouter) {
            this.queryRouter = queryRouter;
            return this;
        }

        public AdiKnowledgeBaseRetrievalAugmentor.AdiKnowledgeBaseRetrievalAugmentorBuilder contentAggregator(ContentAggregator contentAggregator) {
            this.contentAggregator = contentAggregator;
            return this;
        }

        public AdiKnowledgeBaseRetrievalAugmentor.AdiKnowledgeBaseRetrievalAugmentorBuilder contentInjector(ContentInjector contentInjector) {
            this.contentInjector = contentInjector;
            return this;
        }

        public AdiKnowledgeBaseRetrievalAugmentor.AdiKnowledgeBaseRetrievalAugmentorBuilder executor(Executor executor) {
            this.executor = executor;
            return this;
        }

        public AdiKnowledgeBaseRetrievalAugmentor.AdiKnowledgeBaseRetrievalAugmentorBuilder inputAdaptorMsgConsumer(Consumer<InputAdaptorMsg> inputAdaptorMsgConsumer) {
            this.inputAdaptorMsgConsumer = inputAdaptorMsgConsumer;
            return this;
        }

        public AdiKnowledgeBaseRetrievalAugmentor.AdiKnowledgeBaseRetrievalAugmentorBuilder maxInputTokens(int maxInputTokens) {
            this.maxInputTokens = maxInputTokens;
            return this;
        }

        public AdiKnowledgeBaseRetrievalAugmentor build() {
            return new AdiKnowledgeBaseRetrievalAugmentor(this.queryTransformer, this.queryRouter, this.contentAggregator, this.contentInjector, this.executor, this.inputAdaptorMsgConsumer, this.maxInputTokens);
        }

        public String toString() {
            return "AdiKnowledgeBaseRetrievalAugmentor.AdiKnowledgeBaseRetrievalAugmentorBuilder(queryTransformer=" + this.queryTransformer + ", queryRouter=" + this.queryRouter + ", contentAggregator=" + this.contentAggregator + ", contentInjector=" + this.contentInjector + ", executor=" + this.executor + ", inputAdaptorMsgConsumer=" + this.inputAdaptorMsgConsumer + ", maxInputTokens=" + this.maxInputTokens + ")";
        }
    }
}
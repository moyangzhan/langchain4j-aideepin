package com.moyz.adi.common.rag;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.interfaces.IRAGService;
import com.moyz.adi.common.util.InputAdaptor;
import com.moyz.adi.common.util.SpringUtil;
import com.moyz.adi.common.vo.EmbeddingIngestParam;
import com.moyz.adi.common.vo.InputAdaptorMsg;
import com.moyz.adi.common.vo.RetrieverCreateParam;
import dev.langchain4j.data.document.Document;
import dev.langchain4j.data.document.DocumentSplitter;
import dev.langchain4j.data.segment.TextSegment;
import dev.langchain4j.model.embedding.EmbeddingModel;
import dev.langchain4j.store.embedding.EmbeddingStore;
import dev.langchain4j.store.embedding.EmbeddingStoreIngestor;
import dev.langchain4j.store.embedding.filter.Filter;
import dev.langchain4j.store.embedding.filter.comparison.IsNotIn;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import static com.moyz.adi.common.cosntant.AdiConstant.*;
import static com.moyz.adi.common.vo.InputAdaptorMsg.TOKEN_TOO_MUCH_QUESTION;

@Slf4j
public class EmbeddingRag implements IRAGService {

    /**
     * RAG名称，用于区分不同的实例
     */
    @Getter
    private final String name;

    private final EmbeddingModel embeddingModel;

    private final EmbeddingStore<TextSegment> embeddingStore;

    /**
     * 知识库实例的召回后置处理器（懒加载，避免构造期依赖 Spring 容器）
     */
    private volatile RetrievedContentProcessor segmentExpandProcessor;

    /**
     * 语义记忆通道的召回后置处理器（懒加载，同上）
     */
    private volatile RetrievedContentProcessor semanticMemoryExpandProcessor;

    /**
     * 情景记忆通道的召回后置处理器（懒加载，同上）
     */
    private volatile RetrievedContentProcessor episodicMemoryExpandProcessor;

    public EmbeddingRag(String name, EmbeddingModel embeddingModel, EmbeddingStore<TextSegment> embeddingStore) {
        this.name = name;
        this.embeddingModel = embeddingModel;
        this.embeddingStore = embeddingStore;
    }

    /**
     * 对文档切块、向量化并存储到数据库
     *
     * @param document 知识库文档
     * @param params   入库配置参数
     */
    @Override
    public void ingest(Document document, EmbeddingIngestParam params) {
        log.info("EmbeddingRag ingest, strategy:{}, maxSegmentSize:{}, TokenCountEstimator:{}",
                params.getStrategy(), params.getMaxSegmentSize(), params.getTokenEstimator());
        DocumentSplitter documentSplitter = DocumentSplitterFactory.create(
                params.getStrategy(), params.getMaxSegmentSize(), params.getOverlap(),
                params.getCustomSeparator(), TokenEstimatorFactory.create(params.getTokenEstimator()));
        EmbeddingStoreIngestor embeddingStoreIngestor = EmbeddingStoreIngestor.builder()
                .documentSplitter(documentSplitter)
                .embeddingModel(embeddingModel)
                .embeddingStore(embeddingStore)
                .build();
        embeddingStoreIngestor.ingest(document);
    }

    /**
     * 创建召回器。知识库通道挂段展开处理器（text 直用 / 问题→答案 / 子块→父段上卷去重），
     * 语义与情景记忆通道各挂命中回表处理器（按记忆 uuid 回关系表取内容）；四个检索入口
     * （流式问答、阻塞问答、工作流、角色聊天）均经此处自动生效。
     * <p>
     * Create a retriever. The KB channel wires the segment expand processor; the semantic
     * and episodic memory channels each wire a hydration processor (content fetched back
     * from the relational table by memory uuid). All four retrieval entries (streaming
     * ask, blocking ask, workflow, character chat) pick this up here.
     *
     * @param param 条件
     * @return ContentRetriever
     */
    @Override
    public AdiEmbeddingStoreContentRetriever createRetriever(RetrieverCreateParam param) {
        Filter filter = param.getFilter();
        if (param.getExcludedItemUuids() != null && !param.getExcludedItemUuids().isEmpty()) {
            Filter excludeFilter = new IsNotIn(MetadataKey.KB_ITEM_UUID, param.getExcludedItemUuids());
            filter = filter != null ? Filter.and(filter, excludeFilter) : excludeFilter;
        }
        AdiEmbeddingStoreContentRetriever.AdiEmbeddingStoreContentRetrieverBuilder builder = AdiEmbeddingStoreContentRetriever.builder()
                .embeddingStore(embeddingStore)
                .embeddingModel(embeddingModel)
                .maxResults(param.getMaxResults() <= 0 ? 3 : param.getMaxResults())
                .minScore(param.getMinScore() <= 0 ? RAG_MIN_SCORE : param.getMinScore())
                .filter(filter)
                .breakIfSearchMissed(param.isBreakIfSearchMissed());
        if (AdiConstant.RetrieveContentFrom.KNOWLEDGE_BASE.equals(name)) {
            builder.contentPostProcessor(getSegmentExpandProcessor());
        } else if (AdiConstant.RetrieveContentFrom.CHARACTER_MEMORY.equals(name)) {
            builder.contentPostProcessor(getSemanticMemoryExpandProcessor());
        } else if (AdiConstant.RetrieveContentFrom.CHARACTER_MEMORY_EPISODIC.equals(name)) {
            builder.contentPostProcessor(getEpisodicMemoryExpandProcessor());
        }
        return builder.build();
    }

    private RetrievedContentProcessor getSegmentExpandProcessor() {
        if (null == segmentExpandProcessor) {
            segmentExpandProcessor = SpringUtil.getBean(SegmentExpandProcessor.class);
        }
        return segmentExpandProcessor;
    }

    private RetrievedContentProcessor getSemanticMemoryExpandProcessor() {
        if (null == semanticMemoryExpandProcessor) {
            semanticMemoryExpandProcessor = SpringUtil.getBean(SemanticMemoryExpandProcessor.class);
        }
        return semanticMemoryExpandProcessor;
    }

    private RetrievedContentProcessor getEpisodicMemoryExpandProcessor() {
        if (null == episodicMemoryExpandProcessor) {
            episodicMemoryExpandProcessor = SpringUtil.getBean(EpisodicMemoryExpandProcessor.class);
        }
        return episodicMemoryExpandProcessor;
    }

    /**
     * Calculates the maximum number of documents that can be retrieved based on the model's max input tokens.
     * <br/>Uses the maximum segment size in tokens ({@code RAG_MAX_SEGMENT_SIZE_IN_TOKENS}) from chunking as the factor.
     * <p>maxInputTokens must first subtract the tokens consumed by the user question, chat history, and system message;
     * the remaining space is then converted into the number of documents that can be accommodated.</p>
     *
     * @param userQuestion   the user's question
     * @param maxInputTokens the model's input token limit (maxInputTokens), not the full context window size
     * @param reservedTokens tokens already consumed by chat history and system message, which will be deducted from the budget
     * @return the maximum number of documents that can be retrieved (0 means no remaining space)
     */
    public static int getRetrieveMaxResults(String userQuestion, int maxInputTokens, int reservedTokens) {
        if (maxInputTokens == 0) {
            return RAG_RETRIEVE_NUMBER_MAX;
        }
        InputAdaptorMsg inputAdaptorMsg = InputAdaptor.isQuestionValid(userQuestion, maxInputTokens);
        if (inputAdaptorMsg.getTokenTooMuch() == TOKEN_TOO_MUCH_QUESTION) {
            log.warn("User question too long, not enough tokens left for retrieved content");
            return 0;
        }
        int maxRetrieveDocLength = Math.max(0,
                maxInputTokens - inputAdaptorMsg.getUserQuestionTokenCount() - reservedTokens);
        if (maxRetrieveDocLength > RAG_RETRIEVE_NUMBER_MAX * RAG_MAX_SEGMENT_SIZE_IN_TOKENS) {
            return RAG_RETRIEVE_NUMBER_MAX;
        }
        return maxRetrieveDocLength / RAG_MAX_SEGMENT_SIZE_IN_TOKENS;
    }
}

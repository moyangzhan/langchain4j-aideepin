package com.moyz.adi.common.rag;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.dto.RefGraphDto;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.util.AdiStringUtil;
import com.moyz.adi.common.vo.*;
import dev.langchain4j.model.chat.ChatModel;
import dev.langchain4j.rag.content.Content;
import dev.langchain4j.rag.content.retriever.ContentRetriever;
import dev.langchain4j.rag.query.Query;
import dev.langchain4j.store.embedding.filter.Filter;
import lombok.Builder;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Triple;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.moyz.adi.common.enums.ErrorEnum.B_BREAK_SEARCH;
import static dev.langchain4j.internal.Utils.getOrDefault;
import static dev.langchain4j.internal.ValidationUtils.ensureGreaterThanZero;
import static dev.langchain4j.internal.ValidationUtils.ensureNotNull;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;

@Slf4j
public class GraphStoreContentRetriever implements ContentRetriever {
    public static final Function<Query, Integer> DEFAULT_MAX_RESULTS = query -> 3;
    public static final Function<Query, Filter> DEFAULT_FILTER = query -> null;

    public static final String DEFAULT_DISPLAY_NAME = "Default";

    private final GraphStore graphStore;
    private final ChatModel chatModel;

    private final Function<Query, Integer> maxResultsProvider;
    private final Function<Query, Filter> filterProvider;

    private final String displayName;

    private final boolean breakIfSearchMissed;

    private final RefGraphDto kbQaRecordRefGraphDto = RefGraphDto.builder().vertices(Collections.emptyList()).edges(Collections.emptyList()).entitiesFromQuestion(Collections.emptyList()).build();

    @Builder
    private GraphStoreContentRetriever(String displayName,
                                       GraphStore graphStore,
                                       ChatModel chatModel,
                                       Function<Query, Integer> dynamicMaxResults,
                                       Function<Query, Filter> dynamicFilter,
                                       Boolean breakIfSearchMissed) {
        this.displayName = getOrDefault(displayName, DEFAULT_DISPLAY_NAME);
        this.graphStore = ensureNotNull(graphStore, "graphStore");
        this.chatModel = ensureNotNull(chatModel, "ChatModel");
        this.maxResultsProvider = getOrDefault(dynamicMaxResults, DEFAULT_MAX_RESULTS);
        this.filterProvider = getOrDefault(dynamicFilter, DEFAULT_FILTER);
        this.breakIfSearchMissed = breakIfSearchMissed;
    }

    public static GraphStoreContentRetriever from(GraphStore graphStore) {
        return builder().graphStore(graphStore).build();
    }

    @Override
    public List<Content> retrieve(Query query) {
        log.info("Graph retrieve,query:{}", query);
        String response = "";
        try {
            response = chatModel.chat(GraphExtractPrompt.GRAPH_EXTRACTION_PROMPT.replace("{input_text}", query.text()));
        } catch (Exception e) {
            log.error("Graph retrieve. extract graph error", e);
        }
        if (StringUtils.isBlank(response)) {
            return Collections.emptyList();
        }
        Set<String> entities = new HashSet<>();
        String[] records = response.split(AdiConstant.GRAPH_RECORD_DELIMITER);
        for (String record : records) {
            String newRecord = record.replaceAll("^\\(|\\)$", "");
            String[] recordAttributes = newRecord.split(AdiConstant.GRAPH_TUPLE_DELIMITER);
            if (recordAttributes.length >= 4 && (recordAttributes[0].contains("\"entity\"") || recordAttributes[0].contains("\"实体\""))) {
                entities.add(AdiStringUtil.clearStr(recordAttributes[1].toUpperCase()));
            } else if (recordAttributes.length >= 4 && (recordAttributes[0].contains("\"relationship\"") || recordAttributes[0].contains("\"关系\""))) {
                String sourceName = AdiStringUtil.clearStr(recordAttributes[1].toUpperCase());
                String targetName = AdiStringUtil.clearStr(recordAttributes[2].toUpperCase());
                entities.add(AdiStringUtil.clearStr(sourceName));
                entities.add(AdiStringUtil.clearStr(targetName));
            }
        }
        //判断是否要强行中断查询，没有命中则不再进行下一步操作（比如说请求LLM），直接抛出异常中断流程
        if (breakIfSearchMissed && entities.isEmpty()) {
            throw new BaseException(B_BREAK_SEARCH);
        }
        entities = entities.stream().map(AdiStringUtil::removeSpecialChar).filter(StringUtils::isNotBlank).collect(Collectors.toSet());
        if (entities.isEmpty()) {
            log.info("从用户查询中没有解析出实体");
            return Collections.emptyList();
        }

        List<String> entityNames = entities.stream().toList();
        List<GraphVertex> vertices = graphStore.searchVertices(
                GraphVertexSearch.builder()
                        .names(entityNames)
                        .metadataFilter(filterProvider.apply(query))
                        .limit(maxResultsProvider.apply(query))
                        .build()
        );
        List<Triple<GraphVertex, GraphEdge, GraphVertex>> edgeWithVerticeList = graphStore.searchEdges(
                GraphEdgeSearch.builder()
                        .edge(GraphSearchCondition.builder().metadataFilter(filterProvider.apply(query)).build())
                        .limit(maxResultsProvider.apply(query))
                        .build()
        );

        Map<String, GraphVertex> allVertices = new HashMap<>();
        List<GraphEdge> allEdges = new ArrayList<>();
        for (Triple<GraphVertex, GraphEdge, GraphVertex> triple : edgeWithVerticeList) {
            allVertices.put(triple.getLeft().getId(), triple.getLeft());
            allVertices.put(triple.getRight().getId(), triple.getRight());
            allEdges.add(triple.getMiddle());
        }
        allVertices.putAll(vertices.stream().collect(toMap(GraphVertex::getId, Function.identity())));
        kbQaRecordRefGraphDto.setEntitiesFromQuestion(entityNames);
        kbQaRecordRefGraphDto.setVertices(allVertices.values().stream().toList());
        kbQaRecordRefGraphDto.setEdges(allEdges);

        List<Content> vertexContents = vertices.stream().map(GraphVertex::getDescription).map(Content::from).collect(toList());
        List<Content> edgeContents = edgeWithVerticeList.stream().map(Triple::getMiddle).map(GraphEdge::getDescription).map(Content::from).toList();
        vertexContents.addAll(edgeContents);
        return vertexContents;
    }

    public RefGraphDto getGraphRef() {
        return kbQaRecordRefGraphDto;
    }

    public static class GraphStoreContentRetrieverBuilder {

        public GraphStoreContentRetrieverBuilder maxResults(Integer maxResults) {
            if (maxResults != null) {
                dynamicMaxResults = (query) -> ensureGreaterThanZero(maxResults, "maxResults");
            }
            return this;
        }

        public GraphStoreContentRetrieverBuilder filter(Filter filter) {
            if (filter != null) {
                dynamicFilter = (query) -> filter;
            }
            return this;
        }

        public GraphStoreContentRetrieverBuilder breakIfSearchMissed(boolean breakFlag) {
            breakIfSearchMissed = breakFlag;
            return this;
        }
    }

    @Override
    public String toString() {
        return "GraphStoreContentRetriever{" +
               "displayName='" + displayName + '\'' +
               '}';
    }

}

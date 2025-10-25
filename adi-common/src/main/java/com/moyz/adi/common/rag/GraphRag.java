package com.moyz.adi.common.rag;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.KnowledgeBaseGraphSegment;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.helper.QuotaHelper;
import com.moyz.adi.common.service.KnowledgeBaseGraphSegmentService;
import com.moyz.adi.common.service.UserDayCostService;
import com.moyz.adi.common.util.SpringUtil;
import com.moyz.adi.common.util.UuidUtil;
import com.moyz.adi.common.vo.GraphIngestParams;
import com.moyz.adi.common.vo.RetrieverCreateParam;
import dev.langchain4j.data.document.DocumentSplitter;
import dev.langchain4j.data.document.splitter.DocumentSplitters;
import dev.langchain4j.data.message.UserMessage;
import dev.langchain4j.data.segment.TextSegment;
import dev.langchain4j.model.chat.response.ChatResponse;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Triple;

import java.util.ArrayList;
import java.util.List;

import static com.moyz.adi.common.cosntant.AdiConstant.RAG_MAX_SEGMENT_SIZE_IN_TOKENS;

/**
 * 知识图谱RAG，基于图谱存储进行问答增强
 */
@Slf4j
public class GraphRag {

    /**
     * RAG名称，用于区分不同实例
     */
    @Getter
    private final String name;

    private final GraphStore graphStore;

    private KnowledgeBaseGraphSegmentService knowledgeBaseGraphSegmentService;

    public GraphRag(String name, GraphStore kbGraphStore) {
        this.name = name;
        this.graphStore = kbGraphStore;
    }

    public KnowledgeBaseGraphSegmentService getKnowledgeBaseGraphSegmentService() {
        if (null == knowledgeBaseGraphSegmentService) {
            knowledgeBaseGraphSegmentService = SpringUtil.getBean(KnowledgeBaseGraphSegmentService.class);
        }
        return knowledgeBaseGraphSegmentService;
    }

    public void ingest(GraphIngestParams graphIngestParams) {
        log.info("GraphRag ingest");
        User user = graphIngestParams.getUser();
        DocumentSplitter documentSplitter = DocumentSplitters.recursive(RAG_MAX_SEGMENT_SIZE_IN_TOKENS, graphIngestParams.getOverlap(), TokenEstimatorFactory.create(graphIngestParams.getTokenEstimator()));
        GraphStoreIngestor ingestor = GraphStoreIngestor.builder()
                .documentSplitter(documentSplitter)
                .segmentsFunction(segments -> {
                    List<Triple<TextSegment, String, String>> segmentIdToExtractContent = new ArrayList<>();
                    for (TextSegment segment : segments) {

                        String segmentId = UuidUtil.createShort();
                        log.info("Save segment to graph_segment,segmentId:{}", segmentId);
                        KnowledgeBaseGraphSegment graphSegment = new KnowledgeBaseGraphSegment();
                        graphSegment.setUuid(segmentId);
                        graphSegment.setRemark(segment.text());
                        graphSegment.setKbUuid(segment.metadata().getString(AdiConstant.MetadataKey.KB_UUID));
                        graphSegment.setKbItemUuid(segment.metadata().getString(AdiConstant.MetadataKey.KB_ITEM_UUID));
                        graphSegment.setUserId(user.getId());
                        getKnowledgeBaseGraphSegmentService().save(graphSegment);

                        String response = "";
                        if (StringUtils.isNotBlank(segment.text())) {
                            if (!graphIngestParams.isFreeToken()) {
                                ErrorEnum errorMsg = SpringUtil.getBean(QuotaHelper.class).checkTextQuota(user);
                                if (null != errorMsg) {
                                    log.warn("抽取知识图谱时发现额度已超过限制,user:{},errorInfo:{}", user.getName(), errorMsg.getInfo());
                                    continue;
                                }
                            }
                            log.info("请求LLM从文本中抽取实体及关系,segmentId:{}", segmentId);
                            ChatResponse aiMessageResponse = graphIngestParams.getChatModel().chat(UserMessage.from(GraphExtractPrompt.GRAPH_EXTRACTION_PROMPT.replace("{input_text}", segment.text())));
                            response = aiMessageResponse.aiMessage().text();

                            SpringUtil.getBean(UserDayCostService.class).appendCostToUser(user, aiMessageResponse.tokenUsage().totalTokenCount(), graphIngestParams.isFreeToken());
                        }
                        segmentIdToExtractContent.add(Triple.of(segment, segmentId, response));
                    }
                    return segmentIdToExtractContent;
                })
                .identifyColumns(graphIngestParams.getIdentifyColumns())
                .appendColumns(graphIngestParams.getAppendColumns())
                .graphStore(graphStore)
                .build();
        ingestor.ingest(graphIngestParams.getDocument());
    }

    public GraphStoreContentRetriever createRetriever(RetrieverCreateParam param) {
        return GraphStoreContentRetriever.builder()
                .graphStore(graphStore)
                .chatModel(param.getChatModel())
                .maxResults(param.getMaxResults())
                .filter(param.getFilter())
                .breakIfSearchMissed(param.isBreakIfSearchMissed())
                .build();
    }
}

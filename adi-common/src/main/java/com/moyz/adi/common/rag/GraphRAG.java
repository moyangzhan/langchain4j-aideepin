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
import dev.langchain4j.data.document.DocumentSplitter;
import dev.langchain4j.data.document.splitter.DocumentSplitters;
import dev.langchain4j.data.message.AiMessage;
import dev.langchain4j.data.message.UserMessage;
import dev.langchain4j.data.segment.TextSegment;
import dev.langchain4j.model.chat.ChatLanguageModel;
import dev.langchain4j.model.output.Response;
import dev.langchain4j.store.embedding.filter.Filter;
import dev.langchain4j.store.embedding.filter.comparison.IsEqualTo;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Triple;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static com.moyz.adi.common.cosntant.AdiConstant.RAG_MAX_SEGMENT_SIZE_IN_TOKENS;

@Slf4j
public class GraphRAG {

    private final GraphStore graphStore;

    private KnowledgeBaseGraphSegmentService knowledgeBaseGraphSegmentService;

    public GraphRAG(GraphStore kbGraphStore) {
        this.graphStore = kbGraphStore;
    }

    public KnowledgeBaseGraphSegmentService getKnowledgeBaseGraphSegmentService() {
        if (null == knowledgeBaseGraphSegmentService) {
            knowledgeBaseGraphSegmentService = SpringUtil.getBean(KnowledgeBaseGraphSegmentService.class);
        }
        return knowledgeBaseGraphSegmentService;
    }

    public void ingest(GraphIngestParams graphIngestParams) {
        log.info("GraphRAG ingest");
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
                            Response<AiMessage> aiMessageResponse = graphIngestParams.getChatLanguageModel().generate(UserMessage.from(GraphExtractPrompt.GRAPH_EXTRACTION_PROMPT_CN.replace("{input_text}", segment.text())));
                            response = aiMessageResponse.content().text();

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

    public GraphStoreContentRetriever createRetriever(ChatLanguageModel chatLanguageModel, Map<String, String> metadataCond, int maxResults, boolean breakIfSearchMissed) {
        Filter filter = null;
        for (Map.Entry<String, String> entry : metadataCond.entrySet()) {
            String key = entry.getKey();
            String value = entry.getValue();
            if (null == filter) {
                filter = new IsEqualTo(key, value);
            } else {
                filter = filter.and(new IsEqualTo(key, value));
            }
        }
        return GraphStoreContentRetriever.builder()
                .graphStore(graphStore)
                .chatLanguageModel(chatLanguageModel)
                .maxResults(maxResults)
                .filter(filter)
                .breakIfSearchMissed(breakIfSearchMissed)
                .build();
    }
}

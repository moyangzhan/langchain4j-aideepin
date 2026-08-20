package com.moyz.adi.common.rag;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.DocumentSegment;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.helper.QuotaHelper;
import com.moyz.adi.common.service.UserDayCostService;
import com.moyz.adi.common.util.SpringUtil;
import com.moyz.adi.common.vo.GraphIngestParam;
import com.moyz.adi.common.vo.RetrieverCreateParam;
import dev.langchain4j.data.message.UserMessage;
import dev.langchain4j.data.segment.TextSegment;
import dev.langchain4j.data.document.Metadata;
import dev.langchain4j.model.chat.response.ChatResponse;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Triple;

import java.util.ArrayList;
import java.util.List;

/**
 * 知识图谱RAG，基于图谱存储进行问答增强。
 * <p>
 * 图谱与向量两条索引对称挂在同一份 document_segment 段上：图谱抽取的对象即主表段内容
 * （text 段 / qa 答案 / parent_child 父段），顶点/边的 textSegmentId 直接使用段 uuid，
 * 不再独立切分，也不再写 adi_knowledge_base_graph_segment（该表停写、仅保留历史数据）。
 */
@Slf4j
public class GraphRag {

    /**
     * RAG名称，用于区分不同的实例
     */
    @Getter
    private final String name;

    private final GraphStore graphStore;

    public GraphRag(String name, GraphStore kbGraphStore) {
        this.name = name;
        this.graphStore = kbGraphStore;
    }

    public void ingest(GraphIngestParam graphIngestParam) {
        log.info("GraphRag ingest, segments:{}", graphIngestParam.getSegments() == null ? 0 : graphIngestParam.getSegments().size());
        User user = graphIngestParam.getUser();
        List<Triple<TextSegment, String, String>> extracted = new ArrayList<>();
        for (DocumentSegment segment : graphIngestParam.getSegments()) {
            if (StringUtils.isBlank(segment.getContent())) {
                continue;
            }
            Metadata metadata = new Metadata();
            metadata.put(AdiConstant.MetadataKey.KB_UUID, segment.getKbUuid());
            metadata.put(AdiConstant.MetadataKey.KB_ITEM_UUID, segment.getDocUuid());
            TextSegment textSegment = TextSegment.from(segment.getContent(), metadata);
            // 段 uuid 即图谱顶点/边上的 textSegmentId
            String segmentId = segment.getUuid();

            String response = "";
            if (!graphIngestParam.isFreeToken()) {
                ErrorEnum errorMsg = SpringUtil.getBean(QuotaHelper.class).checkTextQuota(user);
                if (null != errorMsg) {
                    log.warn("Quota exceeded during knowledge graph extraction, user:{}, errorInfo:{}", user.getName(), SpringUtil.getMessage(errorMsg.getInfo()));
                    continue;
                }
            }
            log.info("Requesting LLM to extract entities and relations from text, segmentId:{}", segmentId);
            ChatResponse aiMessageResponse = graphIngestParam.getChatModel().chat(UserMessage.from(GraphExtractPrompt.GRAPH_EXTRACTION_PROMPT.replace("{input_text}", segment.getContent())));
            response = aiMessageResponse.aiMessage().text();

            SpringUtil.getBean(UserDayCostService.class).appendCostToUser(user, aiMessageResponse.tokenUsage().totalTokenCount(), graphIngestParam.isFreeToken());
            extracted.add(Triple.of(textSegment, segmentId, response));
        }
        GraphStoreIngestor ingestor = GraphStoreIngestor.builder()
                .identifyColumns(graphIngestParam.getIdentifyColumns())
                .appendColumns(graphIngestParam.getAppendColumns())
                .graphStore(graphStore)
                .build();
        ingestor.ingestExtracted(extracted);
    }

    public GraphStoreContentRetriever createRetriever(RetrieverCreateParam param) {
        return GraphStoreContentRetriever.builder()
                .graphStore(graphStore)
                .chatModel(param.getChatModel())
                .maxResults(param.getMaxResults())
                .filter(param.getFilter())
                .breakIfSearchMissed(param.isBreakIfSearchMissed())
                .excludedItemUuids(param.getExcludedItemUuids())
                .build();
    }
}

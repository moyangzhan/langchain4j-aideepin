package com.moyz.adi.common.rag;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.DocumentSegment;
import com.moyz.adi.common.entity.LLMCallRecord;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.enums.LLMCallRecordSourceType;
import com.moyz.adi.common.exception.IndexTaskCancelledException;
import com.moyz.adi.common.helper.QuotaHelper;
import com.moyz.adi.common.service.LLMCallRecordService;
import com.moyz.adi.common.service.UserDayCostService;
import com.moyz.adi.common.util.SpringUtil;
import com.moyz.adi.common.util.UuidUtil;
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
        long startTime = System.currentTimeMillis();
        int totalInputTokens = 0;
        int totalOutputTokens = 0;
        List<Triple<TextSegment, String, String>> extracted = new ArrayList<>();
        for (DocumentSegment segment : graphIngestParam.getSegments()) {
            // cooperative cancellation checkpoint (same contract as the embedding batches):
            // a superseded graph task must not keep burning LLM tokens on stale content
            if (graphIngestParam.getCancelSignal() != null
                    && Boolean.TRUE.equals(graphIngestParam.getCancelSignal().get())) {
                throw new IndexTaskCancelledException("Index version advanced during graph extraction, docUuid:" + segment.getDocUuid());
            }
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
            if (aiMessageResponse.tokenUsage() != null) {
                totalInputTokens += aiMessageResponse.tokenUsage().inputTokenCount() == null ? 0 : aiMessageResponse.tokenUsage().inputTokenCount();
                totalOutputTokens += aiMessageResponse.tokenUsage().outputTokenCount() == null ? 0 : aiMessageResponse.tokenUsage().outputTokenCount();
            }
            extracted.add(Triple.of(textSegment, segmentId, response));
        }
        GraphStoreIngestor ingestor = GraphStoreIngestor.builder()
                .identifyColumns(graphIngestParam.getIdentifyColumns())
                .appendColumns(graphIngestParam.getAppendColumns())
                .graphStore(graphStore)
                .build();
        ingestor.ingestExtracted(extracted);
        saveCallRecord(graphIngestParam, user, totalInputTokens, totalOutputTokens, System.currentTimeMillis() - startTime);
    }

    /**
     * 图谱抽取的 LLM 调用记录（可观测性，Token Monitor 展示）：与 QA 生成（DocumentQaService）
     * 同款模式，sourceType 为 KNOWLEDGE_BASE_INGEST。此前仅扣减日额度、不落调用记录，属存量缺口。
     */
    private void saveCallRecord(GraphIngestParam graphIngestParam, User user, int inputTokens, int outputTokens, long durationMs) {
        if (user == null || (inputTokens + outputTokens) <= 0) {
            return;
        }
        LLMCallRecord callRecord = new LLMCallRecord();
        callRecord.setUuid(UuidUtil.createShort());
        callRecord.setSourceType(LLMCallRecordSourceType.KNOWLEDGE_BASE_INGEST.getValue());
        callRecord.setSourceId(graphIngestParam.getSourceId());
        callRecord.setUserId(user.getId());
        callRecord.setModelPlatform(graphIngestParam.getModelPlatform());
        callRecord.setModelName(graphIngestParam.getModelName());
        callRecord.setInputTokens(inputTokens);
        callRecord.setOutputTokens(outputTokens);
        callRecord.setDuration((int) durationMs);
        SpringUtil.getBean(LLMCallRecordService.class).saveAsync(callRecord);
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

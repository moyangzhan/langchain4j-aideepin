package com.moyz.adi.common.service;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.DocumentSegment;
import com.moyz.adi.common.entity.DocumentSegmentChildChunk;
import com.moyz.adi.common.entity.DocumentSegmentQuestion;
import com.moyz.adi.common.entity.KbDocument;
import com.moyz.adi.common.entity.KnowledgeBase;
import com.moyz.adi.common.enums.SegmentModeEnum;
import com.moyz.adi.common.exception.IndexTaskCancelledException;
import com.moyz.adi.common.rag.DocumentSplitterFactory;
import com.moyz.adi.common.rag.TokenEstimatorFactory;
import com.moyz.adi.common.service.embedding.IKnowledgeEmbeddingService;
import com.moyz.adi.common.util.UuidUtil;
import dev.langchain4j.data.document.DefaultDocument;
import dev.langchain4j.data.document.Document;
import dev.langchain4j.data.document.DocumentSplitter;
import dev.langchain4j.data.document.Metadata;
import dev.langchain4j.data.embedding.Embedding;
import dev.langchain4j.data.segment.TextSegment;
import dev.langchain4j.model.embedding.EmbeddingModel;
import dev.langchain4j.store.embedding.EmbeddingStore;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.function.Supplier;
import java.util.stream.Collectors;

/**
 * 分段索引编排：切段显式化 + 三种模式的向量化。
 * <p>
 * 职责分离：document_segment.content 是返回给 LLM 的内容（text 分段/qa 答案/父段），
 * 被向量化的内容是 text 主表行 / qa 问题行 / parent_child 子块行。
 * 向量表只存向量与 metadata，TextSegment 文本置空——内容唯一事实源在关系表。
 */
@Slf4j
@Service
public class SegmentIndexService {

    /**
     * 向量化分批大小，避免一次 embedAll 触发远程 embedding 接口的批量上限
     */
    private static final int EMBED_BATCH_SIZE = 32;

    private static final int DEFAULT_CHILD_MAX_CHUNK_SIZE = 200;

    @Resource
    private DocumentSegmentService documentSegmentService;

    @Resource
    private DocumentSegmentQuestionService questionService;

    @Resource
    private DocumentSegmentChildChunkService childChunkService;

    @Resource
    private IKnowledgeEmbeddingService iKnowledgeEmbeddingService;

    @Resource
    private EmbeddingModel embeddingModel;

    @Resource
    private EmbeddingStore<TextSegment> kbEmbeddingStore;

    /**
     * 文档生效的分段模式；历史数据/未设置为 text
     */
    public static SegmentModeEnum effectiveMode(KbDocument doc) {
        return doc.getSegmentMode() == null ? SegmentModeEnum.TEXT : doc.getSegmentMode();
    }

    public void reindexEmbedding(KnowledgeBase kb, KbDocument doc) {
        reindexEmbedding(kb, doc, null);
    }

    /**
     * 重建向量化索引。段行不存在时切段（全量、新段一律默认启用）；段行已存在时为
     * 增量重索引：不重建段行，按状态逐段处理--启用段重新向量化，停用段跳过（不复活）。
     * cancelSignal 非空时在每个 embed 批次前检查，true 即抛 IndexTaskCancelledException
     * （任务队列的协作式作废检查点）。
     */
    public void reindexEmbedding(KnowledgeBase kb, KbDocument doc, Supplier<Boolean> cancelSignal) {
        SegmentModeEnum mode = effectiveMode(doc);
        log.info("reindexEmbedding, docUuid:{}, mode:{}", doc.getUuid(), mode.getValue());
        // 清旧向量（按 metadata kb_item_uuid 过滤删除；停用段无向量，删除天然空转）
        iKnowledgeEmbeddingService.deleteByItemUuid(doc.getUuid());
        ensureSegments(kb, doc);
        switch (mode) {
            case TEXT -> {
                // 启用段置空 embedding_id 待重嵌；停用段保持无向量状态（停用时已置空）
                documentSegmentService.listEnabledByDocUuid(doc.getUuid()).forEach(row -> {
                    if (row.getEmbeddingId() != null) {
                        documentSegmentService.updateEmbeddingId(row.getId(), null);
                    }
                });
            }
            case QA -> questionService.clearEmbeddingIds(doc.getUuid());
            case PARENT_CHILD -> {
                // 启用父段下子块置空待重嵌；停用父段下子块跳过
                List<Long> enabledParentIds = documentSegmentService.listEnabledByDocUuid(doc.getUuid()).stream()
                        .map(DocumentSegment::getId)
                        .toList();
                childChunkService.clearEmbeddingIdsByParentIds(enabledParentIds);
            }
        }
        vectorizePending(kb, doc, mode, cancelSignal);
    }

    /**
     * 图谱抽取前确保段行存在并返回主表段列表。
     * embedding 重跑会重建段行；仅图谱重跑时复用现有段（无段行时先切段）。
     */
    public List<DocumentSegment> ensureSegments(KnowledgeBase kb, KbDocument doc) {
        List<DocumentSegment> segments = documentSegmentService.listByDocUuid(doc.getUuid());
        if (segments.isEmpty()) {
            splitIntoSegments(kb, doc);
            segments = documentSegmentService.listByDocUuid(doc.getUuid());
        }
        return segments;
    }

    /**
     * 按模式把文档 remark 物化为主表段行（qa 模式的段由问答数据流创建，不在此切分）
     */
    public void splitIntoSegments(KnowledgeBase kb, KbDocument doc) {
        SegmentModeEnum mode = effectiveMode(doc);
        switch (mode) {
            case TEXT -> splitText(kb, doc);
            case PARENT_CHILD -> splitParentChild(kb, doc);
            case QA -> log.info("QA mode document {} does not split from remark; segments are created by QA flows", doc.getUuid());
        }
    }

    private void splitText(KnowledgeBase kb, KbDocument doc) {
        Document document = new DefaultDocument(doc.getRemark(), baseMetadata(kb, doc));
        DocumentSplitter splitter = createSplitter(kb, kb.getIngestMaxSegmentSize());
        List<DocumentSegment> rows = new ArrayList<>();
        int position = 0;
        for (TextSegment chunk : splitter.split(document)) {
            if (StringUtils.isBlank(chunk.text())) {
                continue;
            }
            rows.add(newSegmentRow(kb, doc, position++, chunk.text()));
        }
        documentSegmentService.saveBatch(rows);
    }

    private void splitParentChild(KnowledgeBase kb, KbDocument doc) {
        Document document = new DefaultDocument(doc.getRemark(), baseMetadata(kb, doc));
        DocumentSplitter parentSplitter = createSplitter(kb, kb.getIngestMaxSegmentSize());
        DocumentSplitter childSplitter = createSplitter(kb, childMaxChunkSize(doc));
        List<DocumentSegmentChildChunk> children = new ArrayList<>();
        int parentPosition = 0;
        for (TextSegment parentText : parentSplitter.split(document)) {
            if (StringUtils.isBlank(parentText.text())) {
                continue;
            }
            // 逐条保存以回填父段id，供子块引用
            DocumentSegment parent = newSegmentRow(kb, doc, parentPosition++, parentText.text());
            documentSegmentService.save(parent);
            int childPosition = 0;
            for (TextSegment childText : childSplitter.split(new DefaultDocument(parentText.text(), parentText.metadata()))) {
                if (StringUtils.isBlank(childText.text())) {
                    continue;
                }
                children.add(newChildChunkRow(kb, doc, parent.getId(), childPosition++, childText.text()));
            }
        }
        childChunkService.saveBatch(children);
    }

    /**
     * 对模式规定的向量化内容（text 主表行 / 问题行 / 子块行）中尚未向量化的部分做嵌入入库。
     * 存入向量库的 TextSegment 文本置空；embedding 基于真实内容计算。
     */
    private void vectorizePending(KnowledgeBase kb, KbDocument doc, SegmentModeEnum mode, Supplier<Boolean> cancelSignal) {
        // 攒批后一次 embedAndStore，内部再按 EMBED_BATCH_SIZE 分批，避免逐条调用 embedding 接口。
        // 停用段过滤：增量重索引（段行保留）场景下停用段不参与重嵌，避免停用数据"复活"
        List<PendingVector> pending = new ArrayList<>();
        switch (mode) {
            case TEXT -> documentSegmentService.listByDocUuid(doc.getUuid()).stream()
                    .filter(row -> row.getEmbeddingId() == null && !Boolean.FALSE.equals(row.getIsEnabled()))
                    .forEach(row -> pending.add(new PendingVector(row.getUuid(), row.getContent(),
                            id -> documentSegmentService.updateEmbeddingId(row.getId(), id))));
            case QA -> {
                // 重跑向量化时 clearEmbeddingIds 已把全部问题置空，须过滤停用答案下的问题
                Set<Long> enabledAnswerIds = documentSegmentService.listEnabledIdsByDocUuid(doc.getUuid());
                questionService.listByDocUuid(doc.getUuid()).stream()
                        .filter(q -> q.getEmbeddingId() == null && enabledAnswerIds.contains(q.getAnswerSegmentId()))
                        .forEach(q -> pending.add(new PendingVector(q.getUuid(), q.getContent(),
                                id -> questionService.updateEmbeddingId(q.getId(), id))));
            }
            case PARENT_CHILD -> {
                Set<Long> disabledParentIds = documentSegmentService.listByDocUuid(doc.getUuid()).stream()
                        .filter(row -> Boolean.FALSE.equals(row.getIsEnabled()))
                        .map(DocumentSegment::getId)
                        .collect(Collectors.toSet());
                childChunkService.listByDocUuid(doc.getUuid()).stream()
                        .filter(c -> c.getEmbeddingId() == null && !disabledParentIds.contains(c.getParentSegmentId()))
                        .forEach(c -> pending.add(new PendingVector(c.getUuid(), c.getContent(),
                                id -> childChunkService.updateEmbeddingId(c.getId(), id))));
            }
        }
        embedAndStore(kb, doc, pending, cancelSignal);
    }

    /**
     * 单段向量重建（启用分段用）：按模式收集该段名下 embeddingId 为空的待嵌条目
     * （停用时已全部置空）——text=本段；qa=答案下全部问题；parent_child=父段下全部子块。
     */
    public void vectorizeSegment(KnowledgeBase kb, KbDocument doc, DocumentSegment segment) {
        SegmentModeEnum mode = effectiveMode(doc);
        List<PendingVector> pending = new ArrayList<>();
        switch (mode) {
            case TEXT -> {
                if (segment.getEmbeddingId() == null) {
                    pending.add(new PendingVector(segment.getUuid(), segment.getContent(),
                            id -> documentSegmentService.updateEmbeddingId(segment.getId(), id)));
                }
            }
            case QA -> questionService.listByAnswerIds(List.of(segment.getId())).stream()
                    .filter(q -> q.getEmbeddingId() == null)
                    .forEach(q -> pending.add(new PendingVector(q.getUuid(), q.getContent(),
                            id -> questionService.updateEmbeddingId(q.getId(), id))));
            case PARENT_CHILD -> childChunkService.listByParentIds(List.of(segment.getId())).stream()
                    .filter(c -> c.getEmbeddingId() == null)
                    .forEach(c -> pending.add(new PendingVector(c.getUuid(), c.getContent(),
                            id -> childChunkService.updateEmbeddingId(c.getId(), id))));
        }
        embedAndStore(kb, doc, pending);
    }

    /**
     * 对文档下尚未向量化的问题行批量嵌入（QA 导入/LLM 生成完成后调用，不重嵌已有问题）。
     * 过滤停用答案下的问题——新问题挂停用答案时保存但不向量化（由 ManageService 守卫），
     * 此处兜底防止导入/生成批量场景复活停用段。
     */
    public void vectorizePendingQuestions(KnowledgeBase kb, KbDocument doc) {
        Set<Long> enabledAnswerIds = documentSegmentService.listEnabledIdsByDocUuid(doc.getUuid());
        List<PendingVector> pending = questionService.listByDocUuid(doc.getUuid()).stream()
                .filter(q -> q.getEmbeddingId() == null && enabledAnswerIds.contains(q.getAnswerSegmentId()))
                .map(q -> new PendingVector(q.getUuid(), q.getContent(), id -> questionService.updateEmbeddingId(q.getId(), id)))
                .toList();
        embedAndStore(kb, doc, pending);
    }

    private void embedAndStore(KnowledgeBase kb, KbDocument doc, List<PendingVector> items) {
        embedAndStore(kb, doc, items, null);
    }

    /**
     * cancelSignal 非空时每个批次前检查，true 即协作式取消（版本已推进，任务作废）
     */
    private void embedAndStore(KnowledgeBase kb, KbDocument doc, List<PendingVector> items, Supplier<Boolean> cancelSignal) {
        for (int from = 0; from < items.size(); from += EMBED_BATCH_SIZE) {
            if (cancelSignal != null && Boolean.TRUE.equals(cancelSignal.get())) {
                throw new IndexTaskCancelledException("Index version advanced during embedding, docUuid:" + doc.getUuid());
            }
            List<PendingVector> batch = items.subList(from, Math.min(items.size(), from + EMBED_BATCH_SIZE));
            List<String> embeddingIds = batch.stream().map(item -> UUID.randomUUID().toString()).toList();
            List<String> realTexts = batch.stream().map(PendingVector::content).toList();
            // 向量表退化为纯检索索引：TextSegment 文本置空，内容只存关系表
            List<TextSegment> storeSegments = batch.stream()
                    .map(item -> TextSegment.from("", storeMetadata(kb, doc, item.segmentUuid())))
                    .toList();
            List<Embedding> embeddings = embeddingModel.embedAll(realTexts.stream().map(TextSegment::from).toList()).content();
            kbEmbeddingStore.addAll(embeddingIds, embeddings, storeSegments);
            for (int i = 0; i < batch.size(); i++) {
                batch.get(i).embeddingIdSetter().accept(embeddingIds.get(i));
            }
        }
    }

    private DocumentSplitter createSplitter(KnowledgeBase kb, Integer maxSegmentSize) {
        int size = maxSegmentSize == null || maxSegmentSize < 1 ? 1000 : maxSegmentSize;
        return DocumentSplitterFactory.create(
                kb.getIngestSplitStrategy(),
                size,
                kb.getIngestMaxOverlap(),
                kb.getIngestCustomSeparator(),
                TokenEstimatorFactory.create(kb.getIngestTokenEstimator()));
    }

    private int childMaxChunkSize(KbDocument doc) {
        Integer size = doc.getChildMaxChunkSize();
        return size == null || size < 1 ? DEFAULT_CHILD_MAX_CHUNK_SIZE : size;
    }

    private DocumentSegment newSegmentRow(KnowledgeBase kb, KbDocument doc, int position, String content) {
        DocumentSegment row = new DocumentSegment();
        row.setUuid(UuidUtil.createShort());
        row.setKbUuid(kb.getUuid());
        row.setDocUuid(doc.getUuid());
        row.setPosition(position);
        row.setContent(content);
        row.setHitCount(0);
        row.setSource(AdiConstant.SegmentSource.DOC);
        return row;
    }

    private DocumentSegmentChildChunk newChildChunkRow(KnowledgeBase kb, KbDocument doc, Long parentSegmentId, int position, String content) {
        DocumentSegmentChildChunk row = new DocumentSegmentChildChunk();
        row.setUuid(UuidUtil.createShort());
        row.setKbUuid(kb.getUuid());
        row.setDocUuid(doc.getUuid());
        row.setParentSegmentId(parentSegmentId);
        row.setPosition(position);
        row.setContent(content);
        row.setHitCount(0);
        return row;
    }

    /**
     * 切分用 metadata（与旧入库管线保持一致，保证 IsNotIn(KB_ITEM_UUID) 等过滤语义不变）
     */
    private Metadata baseMetadata(KnowledgeBase kb, KbDocument doc) {
        Metadata metadata = new Metadata();
        metadata.put(AdiConstant.MetadataKey.KB_UUID, kb.getUuid());
        metadata.put(AdiConstant.MetadataKey.KB_ITEM_UUID, doc.getUuid());
        return metadata;
    }

    /**
     * 向量库存储用 metadata：在切分 metadata 基础上附段标识
     */
    private Metadata storeMetadata(KnowledgeBase kb, KbDocument doc, String segmentUuid) {
        Metadata metadata = baseMetadata(kb, doc);
        metadata.put(AdiConstant.MetadataKey.SEGMENT_ID, segmentUuid);
        return metadata;
    }

    /**
     * 待向量化条目：真实内容 + 段uuid + 向量条目id回填动作
     */
    private record PendingVector(String segmentUuid, String content, Consumer<String> embeddingIdSetter) {
    }
}

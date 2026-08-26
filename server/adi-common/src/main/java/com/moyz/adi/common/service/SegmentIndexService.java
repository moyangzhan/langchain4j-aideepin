package com.moyz.adi.common.service;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.config.AdiProperties;
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
import com.moyz.adi.common.util.AdiStringUtil;
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
 * Segment index orchestration: explicit splitting plus vectorization across the three segment modes.
 * <p>
 * Separation of concerns: document_segment.content is what gets returned to the LLM (text segment /
 * qa answer / parent chunk), while what gets vectorized is the text main-table row / qa question row /
 * parent_child child-chunk row. The vector store keeps only vectors and metadata (segment uuid as
 * placeholder text) — relational tables are the single source of truth for content.
 */
@Slf4j
@Service
public class SegmentIndexService {

    /**
     * Vectorization batch size; avoids hitting the remote embedding API's per-request limit with a single embedAll
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

    @Resource
    private AdiProperties adiProperties;

    /**
     * Effective segment mode of the document; unset/legacy rows fall back to text
     */
    public static SegmentModeEnum effectiveMode(KbDocument doc) {
        return doc.getSegmentMode() == null ? SegmentModeEnum.TEXT : doc.getSegmentMode();
    }

    public void reindexEmbedding(KnowledgeBase kb, KbDocument doc) {
        reindexEmbedding(kb, doc, null);
    }

    /**
     * Rebuild the vector index. Splits first when segment rows are absent (full rebuild, new segments
     * default to enabled); when segment rows already exist this is an incremental reindex: rows are kept
     * and processed by status — enabled segments are re-embedded, disabled ones skipped (no revival).
     * When cancelSignal is non-null it is checked before each embed batch and a true result throws
     * IndexTaskCancelledException (cooperative cancellation checkpoint of the task queue).
     */
    public void reindexEmbedding(KnowledgeBase kb, KbDocument doc, Supplier<Boolean> cancelSignal) {
        SegmentModeEnum mode = effectiveMode(doc);
        log.info("reindexEmbedding, docUuid:{}, mode:{}", doc.getUuid(), mode.getValue());
        // Drop old vectors (deleted by metadata kb_item_uuid; disabled segments have none, so the delete is a no-op for them)
        iKnowledgeEmbeddingService.deleteByItemUuid(doc.getUuid());
        ensureSegments(kb, doc);
        switch (mode) {
            case TEXT -> {
                // Clear embedding_id of enabled segments for re-embedding; disabled segments stay vector-free (cleared when disabled)
                documentSegmentService.listEnabledByDocUuid(doc.getUuid()).forEach(row -> {
                    if (row.getEmbeddingId() != null) {
                        documentSegmentService.updateEmbeddingId(row.getId(), null);
                    }
                });
            }
            case QA -> questionService.clearEmbeddingIds(doc.getUuid());
            case PARENT_CHILD -> {
                // Clear child chunks under enabled parents for re-embedding; children of disabled parents are skipped
                List<Long> enabledParentIds = documentSegmentService.listEnabledByDocUuid(doc.getUuid()).stream()
                        .map(DocumentSegment::getId)
                        .toList();
                childChunkService.clearEmbeddingIdsByParentIds(enabledParentIds);
            }
        }
        vectorizePending(kb, doc, mode, cancelSignal);
    }

    /**
     * Ensure segment rows exist before graph extraction and return the main-table segment list.
     * Embedding re-runs rebuild segment rows; graph-only re-runs reuse existing segments (splitting first only when absent).
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
     * Materialize the document remark into main-table segment rows by mode (qa-mode segments are created by QA data flows, not split here)
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
            // Save one by one to backfill the parent segment id for child-chunk references
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
     * Embed and store the not-yet-vectorized entries among the mode's vectorization targets
     * (text main-table rows / question rows / child-chunk rows).
     * The TextSegment text stored in the vector store is a placeholder; embeddings are computed from the real content.
     */
    private void vectorizePending(KnowledgeBase kb, KbDocument doc, SegmentModeEnum mode, Supplier<Boolean> cancelSignal) {
        // Batch up and call embedAndStore once, which sub-batches by EMBED_BATCH_SIZE, to avoid per-item embedding API calls.
        // Disabled-segment filtering: in incremental reindex (segment rows kept) disabled segments are not re-embedded, preventing disabled data from being "revived"
        List<PendingVector> pending = new ArrayList<>();
        switch (mode) {
            case TEXT -> documentSegmentService.listByDocUuid(doc.getUuid()).stream()
                    .filter(row -> row.getEmbeddingId() == null && !Boolean.FALSE.equals(row.getIsEnabled()))
                    .forEach(row -> pending.add(new PendingVector(row.getUuid(), row.getContent(),
                            id -> documentSegmentService.updateEmbeddingId(row.getId(), id))));
            case QA -> {
                // clearEmbeddingIds nulled every question on re-run, so questions under disabled answers must be filtered out
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
     * Rebuild vectors of a single segment (used when enabling a segment): collect the segment's
     * entries whose embeddingId is null (all cleared on disable) by mode — text = this segment;
     * qa = all questions under the answer; parent_child = all child chunks under the parent.
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
     * Batch-embed the document's not-yet-vectorized question rows (called after QA import / LLM
     * generation; already-embedded questions are not re-embedded).
     * Questions under disabled answers are filtered out — a new question attached to a disabled
     * answer is saved but not vectorized (guarded by ManageService); this is a backstop for the
     * import/generation bulk paths to prevent reviving disabled segments.
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
     * When cancelSignal is non-null, check before each batch; true triggers cooperative cancellation (version advanced, task obsolete)
     */
    private void embedAndStore(KnowledgeBase kb, KbDocument doc, List<PendingVector> items, Supplier<Boolean> cancelSignal) {
        for (int from = 0; from < items.size(); from += EMBED_BATCH_SIZE) {
            if (cancelSignal != null && Boolean.TRUE.equals(cancelSignal.get())) {
                throw new IndexTaskCancelledException("Index version advanced during embedding, docUuid:" + doc.getUuid());
            }
            List<PendingVector> batch = items.subList(from, Math.min(items.size(), from + EMBED_BATCH_SIZE));
            List<String> embeddingIds = batch.stream().map(item -> UUID.randomUUID().toString()).toList();
            List<String> realTexts = batch.stream().map(PendingVector::content).toList();
            // The vector store is a pure retrieval index: content lives only in relational tables;
            // langchain4j requires non-blank TextSegment text, so the segment uuid serves as the placeholder
            // (the uuid is also kept in metadata; retrieval content is expanded by the post-processor from relational tables, never read from here)
            List<TextSegment> storeSegments = batch.stream()
                    .map(item -> TextSegment.from(item.segmentUuid(), storeMetadata(kb, doc, item.segmentUuid())))
                    .toList();
            List<Embedding> embeddings;
            try {
                embeddings = embeddingModel.embedAll(realTexts.stream().map(TextSegment::from).toList()).content();
            } catch (Exception e) {
                // Sanitize the JSON error body and add the embedding model name; lands in fail_reason
                String friendly = AdiStringUtil.extractJsonMessage(
                        e.getMessage() != null ? e.getMessage() : e.getClass().getSimpleName());
                throw new RuntimeException(friendly + ", name: " + adiProperties.getEmbeddingModel(), e);
            }
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
     * Metadata used for splitting (kept consistent with the legacy ingest pipeline so filters like IsNotIn(KB_ITEM_UUID) keep their semantics)
     */
    private Metadata baseMetadata(KnowledgeBase kb, KbDocument doc) {
        Metadata metadata = new Metadata();
        metadata.put(AdiConstant.MetadataKey.KB_UUID, kb.getUuid());
        metadata.put(AdiConstant.MetadataKey.KB_ITEM_UUID, doc.getUuid());
        return metadata;
    }

    /**
     * Metadata used for vector-store entries: splitting metadata plus the segment identifier
     */
    private Metadata storeMetadata(KnowledgeBase kb, KbDocument doc, String segmentUuid) {
        Metadata metadata = baseMetadata(kb, doc);
        metadata.put(AdiConstant.MetadataKey.SEGMENT_ID, segmentUuid);
        return metadata;
    }

    /**
     * Entry awaiting vectorization: real content + segment uuid + embedding-id backfill action
     */
    private record PendingVector(String segmentUuid, String content, Consumer<String> embeddingIdSetter) {
    }
}

package com.moyz.adi.common.service.embedding.neo4j;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.DocumentSegment;
import com.moyz.adi.common.entity.KnowledgeBase;
import com.moyz.adi.common.rag.neo4j.AdiNeo4jEmbeddingStore;
import com.moyz.adi.common.service.DocumentSegmentService;
import com.moyz.adi.common.service.KnowledgeBaseService;
import com.moyz.adi.common.util.UuidUtil;
import dev.langchain4j.data.segment.TextSegment;
import dev.langchain4j.store.embedding.EmbeddingMatch;
import dev.langchain4j.store.embedding.EmbeddingSearchResult;
import dev.langchain4j.store.embedding.EmbeddingStore;
import dev.langchain4j.store.embedding.filter.comparison.IsEqualTo;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * neo4j 向量后端的存量段回填（对应 pgvector 部署执行的迁移 013 Section 4）。
 * <p>
 * 启动时把 neo4j 知识库向量节点上的存量 text 物化为 adi_document_segment 行（全部 text 模式），
 * 然后清空节点 text——关系层成为唯一事实源。幂等：embedding_id 已存在或 text 已空的节点跳过。
 * 仅在 adi.vector-database=neo4j 时生效；pgvector 部署由 SQL 迁移完成，不运行本组件。
 */
@Slf4j
@Component
@ConditionalOnProperty(value = "adi.vector-database", havingValue = "neo4j")
public class SegmentNeo4jBackfillRunner implements ApplicationRunner {

    private static final int SCAN_LIMIT_PER_KB = 10_000;

    @Resource
    @Qualifier("kbEmbeddingStore")
    private EmbeddingStore<TextSegment> kbEmbeddingStore;

    @Resource
    private KnowledgeBaseService knowledgeBaseService;

    @Resource
    private DocumentSegmentService documentSegmentService;

    @Override
    public void run(ApplicationArguments args) {
        try {
            backfill();
        } catch (Exception e) {
            log.error("Segment neo4j backfill failed; relational tables may be incomplete for legacy nodes", e);
        }
    }

    private void backfill() {
        AdiNeo4jEmbeddingStore store = (AdiNeo4jEmbeddingStore) kbEmbeddingStore;
        List<KnowledgeBase> knowledgeBases = knowledgeBaseService.lambdaQuery()
                .select(KnowledgeBase::getUuid)
                .eq(KnowledgeBase::getIsDeleted, false)
                .list();
        for (KnowledgeBase kb : knowledgeBases) {
            EmbeddingSearchResult<TextSegment> searchResult =
                    store.searchByMetadata(new IsEqualTo(AdiConstant.MetadataKey.KB_UUID, kb.getUuid()), SCAN_LIMIT_PER_KB);
            List<DocumentSegment> rows = new ArrayList<>();
            List<String> backfilledIds = new ArrayList<>();
            int position = 0;
            for (EmbeddingMatch<TextSegment> match : searchResult.matches()) {
                String text = match.embedded().text();
                String docUuid = match.embedded().metadata().getString(AdiConstant.MetadataKey.KB_ITEM_UUID);
                if (StringUtils.isBlank(text) || StringUtils.isBlank(docUuid) || StringUtils.isBlank(match.embeddingId())) {
                    // 已清空/缺 metadata 的节点跳过，天然幂等
                    continue;
                }
                DocumentSegment row = new DocumentSegment();
                row.setUuid(UuidUtil.createShort());
                row.setKbUuid(kb.getUuid());
                row.setDocUuid(docUuid);
                row.setPosition(position++);
                row.setContent(text);
                row.setHitCount(0);
                row.setEmbeddingId(match.embeddingId());
                row.setSource(AdiConstant.SegmentSource.DOC);
                rows.add(row);
                backfilledIds.add(match.embeddingId());
            }
            if (CollectionUtils.isEmpty(rows)) {
                continue;
            }
            // 幂等保护：跳过已回填过的条目
            Set<String> existing = new HashSet<>(
                    documentSegmentService.listByEmbeddingIds(backfilledIds).stream()
                            .map(DocumentSegment::getEmbeddingId)
                            .toList());
            List<DocumentSegment> toInsert = rows.stream()
                    .filter(row -> !existing.contains(row.getEmbeddingId()))
                    .toList();
            List<String> toClear = backfilledIds.stream()
                    .filter(id -> !existing.contains(id))
                    .toList();
            if (!toInsert.isEmpty()) {
                documentSegmentService.saveBatch(toInsert);
            }
            if (!toClear.isEmpty()) {
                store.clearText(toClear);
            }
            log.info("Segment neo4j backfill for kb {}: {} segments inserted, {} node texts cleared",
                    kb.getUuid(), toInsert.size(), toClear.size());
        }
    }
}

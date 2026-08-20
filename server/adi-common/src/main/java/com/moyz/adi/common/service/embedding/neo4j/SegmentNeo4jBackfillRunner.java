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
 * 【兼容性代码｜分段重构的 neo4j 向量后端存量迁移】对应 pgvector 部署执行的迁移 013 Section 4。
 * <p>
 * 目的：分段重构把段内容的事实源从向量库迁到关系表 adi_document_segment，向量节点退化为纯检索
 * 索引（text 置空）。pgvector 部署由 SQL 迁移完成这次搬运；neo4j 向量后端无法用 PostgreSQL
 * 脚本触达，由本组件在应用启动时代为完成同样的搬运。仅在 adi.vector-database=neo4j 时生效。
 * <p>
 * 行为：逐知识库扫描 neo4j 中仍带非空 text 的向量节点，把 text 物化为 text 模式段行
 * （沿用原 embedding_id，检索链接不变），随后清空节点 text。
 * <p>
 * 数据安全约束（顺序不可反）：text 是旧数据的唯一内容来源，清空不可逆。因此必须
 * "先确认关系层已存在对应段行、再清空"——清空名单不是插入前的差集，而是 saveBatch 之后
 * 回查关系层得到的确认集；插入失败抛异常中断，本次不会执行任何清空。幂等与崩溃恢复：
 * text 已空的节点跳过；已插入的条目不重复插入；若上次"插入成功、清空前进程退出"，
 * 本次回查仍会命中这些条目并补清。
 * <p>
 * 删除时机：本类是过渡性代码，迁移完成后可整体删除。判定标准：连续多次启动日志均输出
 * 0 inserted / 0 cleared，且图库中不再存在带非空 text 的知识库向量节点（验证 Cypher，
 * 标签/属性名以 AdiNeo4jEmbeddingStore 配置为准：
 * MATCH (n) WHERE n.text IS NOT NULL AND n.text &lt;&gt; '' RETURN count(n) 为 0）。
 * 在此之前它每次启动只做一次扫描，成本可忽略，保留无害。pgvector 部署不加载本组件。
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
            // 迁移失败不阻断启动：残留 text 的节点下次启动会按幂等语义重试（见类注释）
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
            // 幂等保护：上次运行已插入的条目本次不再重复插入
            Set<String> existing = listConfirmedEmbeddingIds(backfilledIds);
            List<DocumentSegment> toInsert = rows.stream()
                    .filter(row -> !existing.contains(row.getEmbeddingId()))
                    .toList();
            if (!toInsert.isEmpty()) {
                documentSegmentService.saveBatch(toInsert);
            }
            // 先插入、后清空（顺序不可反，见类注释）：清空名单以 saveBatch 之后回查关系层的
            // 确认集为准——同时覆盖本次插入与"上次已插入但未及清空"的遗留条目；上面 saveBatch
            // 失败会抛异常中断，走不到这里，不存在"未落库先清空"的路径
            Set<String> confirmed = listConfirmedEmbeddingIds(backfilledIds);
            List<String> toClear = backfilledIds.stream()
                    .filter(confirmed::contains)
                    .toList();
            if (!toClear.isEmpty()) {
                store.clearText(toClear);
            }
            log.info("Segment neo4j backfill for kb {}: {} segments inserted, {} node texts cleared",
                    kb.getUuid(), toInsert.size(), toClear.size());
        }
    }

    /**
     * 回查关系层：给定 embeddingId 中已存在对应 adi_document_segment 段行的集合
     */
    private Set<String> listConfirmedEmbeddingIds(List<String> embeddingIds) {
        return new HashSet<>(documentSegmentService.listByEmbeddingIds(embeddingIds).stream()
                .map(DocumentSegment::getEmbeddingId)
                .toList());
    }
}

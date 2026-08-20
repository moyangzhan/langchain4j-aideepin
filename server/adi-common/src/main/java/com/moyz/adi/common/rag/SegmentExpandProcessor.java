package com.moyz.adi.common.rag;

import com.moyz.adi.common.entity.DocumentSegment;
import com.moyz.adi.common.entity.DocumentSegmentChildChunk;
import com.moyz.adi.common.entity.DocumentSegmentQuestion;
import com.moyz.adi.common.service.DocumentSegmentChildChunkService;
import com.moyz.adi.common.service.DocumentSegmentQuestionService;
import com.moyz.adi.common.service.DocumentSegmentService;
import dev.langchain4j.data.segment.TextSegment;
import dev.langchain4j.rag.content.Content;
import dev.langchain4j.rag.query.Query;
import dev.langchain4j.store.embedding.EmbeddingMatch;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * 知识库向量命中的"按模式展开"处理器，落地附件检索流程图：
 * <pre>
 * 向量库 ANN 检索 → top-K 混着三种命中（text 分段、qa 问题、parent_child 子块）
 *   → 映射回关系层 segment，按模式展开：
 *       text → 直接用主表行内容
 *       qa   → 取答案内容
 *       parent_child → 上卷父段内容；多个子块命中同一父段时去重合并（保最高分）
 *   → (可选 rerank) → 拼进 prompt
 * </pre>
 * 查不到关系行的命中直接丢弃（向量表 text 已置空，无回退来源）。
 * 只把保留下来的命中写入 embeddingToScore，避免悬空引用与命中统计。
 */
@Slf4j
@Component
public class SegmentExpandProcessor implements RetrievedContentProcessor {

    @Resource
    private DocumentSegmentService documentSegmentService;

    @Resource
    private DocumentSegmentQuestionService questionService;

    @Resource
    private DocumentSegmentChildChunkService childChunkService;

    /**
     * rerank 扩展点：当前只有 NoOp 直通实现，后续接 rerank 模型时替换 Bean 即可
     */
    @Resource
    private ContentReranker contentReranker;

    @Override
    public List<Content> process(Query query, List<EmbeddingMatch<TextSegment>> matches, Map<String, Double> embeddingToScore) {
        if (matches == null || matches.isEmpty()) {
            return Collections.emptyList();
        }
        List<String> embeddingIds = matches.stream().map(EmbeddingMatch::embeddingId).toList();

        Map<String, DocumentSegment> mainByEmb = documentSegmentService.mapByEmbeddingIds(embeddingIds);
        List<DocumentSegmentQuestion> questionHits = questionService.listByEmbeddingIds(embeddingIds);
        Map<String, DocumentSegmentQuestion> questionByEmb = questionHits.stream()
                .filter(q -> q.getEmbeddingId() != null)
                .collect(Collectors.toMap(DocumentSegmentQuestion::getEmbeddingId, Function.identity(), (a, b) -> a));
        List<DocumentSegmentChildChunk> childHits = childChunkService.listByEmbeddingIds(embeddingIds);
        Map<String, DocumentSegmentChildChunk> childByEmb = childHits.stream()
                .filter(c -> c.getEmbeddingId() != null)
                .collect(Collectors.toMap(DocumentSegmentChildChunk::getEmbeddingId, Function.identity(), (a, b) -> a));

        // 二跳：命中问题→答案行、命中子块→父段行
        Set<Long> secondHopIds = new HashSet<>();
        questionHits.forEach(q -> secondHopIds.add(q.getAnswerSegmentId()));
        childHits.forEach(c -> secondHopIds.add(c.getParentSegmentId()));
        Map<Long, DocumentSegment> mainById = documentSegmentService.mapByIds(secondHopIds);

        List<Content> result = new ArrayList<>();
        Set<Long> emittedParents = new HashSet<>();
        // matches 按分数降序；父段去重保留最高分命中
        for (EmbeddingMatch<TextSegment> match : matches) {
            String embeddingId = match.embeddingId();

            DocumentSegment main = mainByEmb.get(embeddingId);
            if (main != null) {
                embeddingToScore.put(embeddingId, match.score());
                result.add(toContent(match, main.getContent()));
                continue;
            }
            DocumentSegmentQuestion question = questionByEmb.get(embeddingId);
            if (question != null) {
                embeddingToScore.put(embeddingId, match.score());
                DocumentSegment answer = mainById.get(question.getAnswerSegmentId());
                // 答案行缺失（脏数据）时退回问题文本，保证不返回空内容
                result.add(toContent(match, answer != null ? answer.getContent() : question.getContent()));
                continue;
            }
            DocumentSegmentChildChunk child = childByEmb.get(embeddingId);
            if (child != null) {
                embeddingToScore.put(embeddingId, match.score());
                if (!emittedParents.contains(child.getParentSegmentId())) {
                    DocumentSegment parent = mainById.get(child.getParentSegmentId());
                    if (parent != null) {
                        emittedParents.add(child.getParentSegmentId());
                        result.add(toContent(match, parent.getContent()));
                    } else {
                        log.warn("Parent segment {} missing for child chunk hit {}", child.getParentSegmentId(), embeddingId);
                    }
                }
                continue;
            }
            // 查不到关系行（脏数据/未迁移）→ 丢弃；向量表 text 已置空，无回退来源
            log.warn("No relational segment row for embedding hit {}, dropped", embeddingId);
        }
        return contentReranker.rerank(query.text(), result);
    }

    /**
     * 用展开后的内容构造 Content，保留原命中的 metadata（kb_uuid/kb_item_uuid/segment_id 等）
     */
    private Content toContent(EmbeddingMatch<TextSegment> match, String expandedText) {
        return Content.from(TextSegment.from(expandedText, match.embedded().metadata()));
    }
}

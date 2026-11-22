package com.moyz.adi.common.rag;

import com.moyz.adi.common.entity.CharacterMemorySemantic;
import com.moyz.adi.common.service.CharacterMemorySemanticService;
import com.moyz.adi.common.util.UuidUtil;
import dev.langchain4j.data.segment.TextSegment;
import dev.langchain4j.rag.content.Content;
import dev.langchain4j.rag.query.Query;
import dev.langchain4j.store.embedding.EmbeddingMatch;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * 语义记忆通道命中回表处理器：向量命中按 embeddingId（=记忆 uuid 的 canonical 形）批量回关系表取内容。
 * 无关系行的命中丢弃并告警，处置与知识库通道一致。
 * <p>
 * Post-processor for the semantic memory channel: batch-hydrates hits back to the relational table by embeddingId (the memory uuid in canonical form).
 * Hits without a relational row are dropped with a warning, same handling as the KB channel.
 */
@Slf4j
@Component
public class SemanticMemoryExpandProcessor implements RetrievedContentProcessor {

    @Resource
    private CharacterMemorySemanticService characterMemorySemanticService;

    @Override
    public List<Content> process(Query query, List<EmbeddingMatch<TextSegment>> matches, Map<String, Double> embeddingToScore) {
        if (matches == null || matches.isEmpty()) {
            return Collections.emptyList();
        }
        List<String> uuids = matches.stream().map(match -> UuidUtil.toShort(match.embeddingId())).toList();
        Map<String, CharacterMemorySemantic> byUuid = characterMemorySemanticService.mapByUuids(uuids);

        List<Content> result = new ArrayList<>();
        for (EmbeddingMatch<TextSegment> match : matches) {
            CharacterMemorySemantic row = byUuid.get(UuidUtil.toShort(match.embeddingId()));
            if (row == null) {
                log.warn("No relational semantic memory row for embedding hit {}, dropped", match.embeddingId());
                continue;
            }
            embeddingToScore.put(match.embeddingId(), match.score());
            result.add(Content.from(TextSegment.from(row.getContent(), match.embedded().metadata())));
        }
        return result;
    }
}

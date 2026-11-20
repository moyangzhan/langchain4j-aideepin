package com.moyz.adi.common.rag;

import com.moyz.adi.common.entity.CharacterMemoryEpisodic;
import com.moyz.adi.common.service.CharacterMemoryEpisodicService;
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
 * 情景记忆通道命中回表处理器：向量命中按 embeddingId（=记忆 uuid 的 canonical 形）批量回
 * 关系表取内容；无关系行的命中丢弃并告警（孤儿语义与知识库通道一致）。
 * <p>
 * Post-processor for the episodic memory channel: batch-hydrates hits back to the
 * relational table by embeddingId (the memory uuid in canonical form). Hits without a
 * relational row are dropped with a warning (same orphan semantics as the KB channel).
 */
@Slf4j
@Component
public class EpisodicMemoryExpandProcessor implements RetrievedContentProcessor {

    @Resource
    private CharacterMemoryEpisodicService characterMemoryEpisodicService;

    @Override
    public List<Content> process(Query query, List<EmbeddingMatch<TextSegment>> matches, Map<String, Double> embeddingToScore) {
        if (matches == null || matches.isEmpty()) {
            return Collections.emptyList();
        }
        List<String> uuids = matches.stream().map(match -> UuidUtil.toShort(match.embeddingId())).toList();
        Map<String, CharacterMemoryEpisodic> byUuid = characterMemoryEpisodicService.mapByUuids(uuids);

        List<Content> result = new ArrayList<>();
        for (EmbeddingMatch<TextSegment> match : matches) {
            CharacterMemoryEpisodic row = byUuid.get(UuidUtil.toShort(match.embeddingId()));
            if (row == null) {
                log.warn("No relational episodic memory row for embedding hit {}, dropped", match.embeddingId());
                continue;
            }
            embeddingToScore.put(match.embeddingId(), match.score());
            result.add(Content.from(TextSegment.from(row.getContent(), match.embedded().metadata())));
        }
        return result;
    }
}

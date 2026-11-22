package com.moyz.adi.common.rag;

import com.moyz.adi.common.entity.CharacterMemoryEpisodic;
import com.moyz.adi.common.service.CharacterMemoryEpisodicService;
import com.moyz.adi.common.util.UuidUtil;
import dev.langchain4j.data.document.Metadata;
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
import java.util.List;
import java.util.Map;

import static com.moyz.adi.common.cosntant.AdiConstant.MetadataKey.CREATE_TIME;

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
            Map<String, Object> enriched = new HashMap<>(match.embedded().metadata().toMap());
            if (row.getCreateTime() != null) {
                enriched.put(CREATE_TIME, row.getCreateTime().toLocalDate().toString());
            } else {
                enriched.remove(CREATE_TIME);
            }
            result.add(Content.from(TextSegment.from(row.getContent(), new Metadata(enriched))));
        }
        return result;
    }
}

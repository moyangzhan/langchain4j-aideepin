package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.dto.KbItemEmbeddingDto;
import com.moyz.adi.common.dto.RefEmbeddingDto;
import com.moyz.adi.common.entity.CharacterEpisodicMemory;
import com.moyz.adi.common.entity.CharacterMessageRefMemoryEmbedding;
import com.moyz.adi.common.mapper.CharacterMessageRefMemoryEmbeddingMapper;
import com.moyz.adi.common.service.embedding.ICharacterMemoryEmbeddingService;
import com.moyz.adi.common.util.EmbeddingUtil;
import com.moyz.adi.common.util.LocalDateTimeUtil;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Slf4j
@Service
public class CharacterMessageRefMemoryEmbeddingService extends ServiceImpl<CharacterMessageRefMemoryEmbeddingMapper, CharacterMessageRefMemoryEmbedding> {

    @Resource
    private ICharacterMemoryEmbeddingService characterMemoryEmbeddingService;

    @Resource
    private CharacterEpisodicMemoryService characterEpisodicMemoryService;

    /**
     * List memory references attached to a message, enriched with memory-type metadata
     * so the frontend can group semantic vs episodic hits in the "Memory" popup.
     * <p>
     * 列出消息引用的记忆，并附带 memory type 元数据，让前端能在"记忆"弹窗中分组展示
     * semantic / episodic。
     */
    public List<RefEmbeddingDto> listRefEmbeddings(String uuid) {
        List<CharacterMessageRefMemoryEmbedding> recordReferences = this.getBaseMapper().listByMsgUuid(uuid);
        if (CollectionUtils.isEmpty(recordReferences)) {
            return Collections.emptyList();
        }
        List<String> embeddingIds = recordReferences.stream().map(CharacterMessageRefMemoryEmbedding::getEmbeddingId).toList();
        if (CollectionUtils.isEmpty(embeddingIds)) {
            return Collections.emptyList();
        }
        List<KbItemEmbeddingDto> embeddings = characterMemoryEmbeddingService.listByEmbeddingIds(embeddingIds);
        List<RefEmbeddingDto> dtos = EmbeddingUtil.itemToRefEmbeddingDto(embeddings);

        // Look up which of these embeddings belong to the episodic table; those are tagged
        // "episodic" with their structured fields, the rest default to "semantic".
        // <p>
        // 用 embedding_id 反查 episodic 表：命中者标记为 episodic 并填充结构化字段，
        // 其余默认 semantic。
        List<CharacterEpisodicMemory> episodicHits = characterEpisodicMemoryService.listByEmbeddingIds(embeddingIds);
        Map<String, CharacterEpisodicMemory> episodicByEmbeddingId = new HashMap<>();
        for (CharacterEpisodicMemory row : episodicHits) {
            episodicByEmbeddingId.put(row.getEmbeddingId(), row);
        }

        for (RefEmbeddingDto dto : dtos) {
            CharacterEpisodicMemory episodic = episodicByEmbeddingId.get(dto.getEmbeddingId());
            if (episodic != null) {
                dto.setMemoryType(AdiConstant.MemoryType.EPISODIC);
                dto.setCreatedAt(episodic.getCreatedAt() != null
                        ? LocalDateTimeUtil.format(episodic.getCreatedAt())
                        : null);
                dto.setEventType(episodic.getEventType());
                dto.setImportance(episodic.getImportance());
            } else {
                dto.setMemoryType(AdiConstant.MemoryType.SEMANTIC);
            }
        }
        return dtos;
    }

}

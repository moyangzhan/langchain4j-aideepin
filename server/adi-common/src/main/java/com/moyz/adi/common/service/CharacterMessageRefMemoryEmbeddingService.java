package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.fasterxml.jackson.databind.JsonNode;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.dto.KbItemEmbeddingDto;
import com.moyz.adi.common.dto.RefEmbeddingDto;
import com.moyz.adi.common.entity.CharacterMessageRefMemoryEmbedding;
import com.moyz.adi.common.mapper.CharacterMessageRefMemoryEmbeddingMapper;
import com.moyz.adi.common.service.embedding.ICharacterMemoryEmbeddingService;
import com.moyz.adi.common.service.embedding.IEpisodicMemoryEmbeddingService;
import com.moyz.adi.common.util.EmbeddingUtil;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.moyz.adi.common.cosntant.AdiConstant.MetadataKey.CREATED_AT;
import static com.moyz.adi.common.cosntant.AdiConstant.MetadataKey.EVENT_TYPE;
import static com.moyz.adi.common.cosntant.AdiConstant.MetadataKey.IMPORTANCE;
import static com.moyz.adi.common.cosntant.AdiConstant.MetadataKey.MEMORY_TYPE;

@Slf4j
@Service
public class CharacterMessageRefMemoryEmbeddingService extends ServiceImpl<CharacterMessageRefMemoryEmbeddingMapper, CharacterMessageRefMemoryEmbedding> {

    @Resource
    private ICharacterMemoryEmbeddingService characterMemoryEmbeddingService;

    @Resource
    private IEpisodicMemoryEmbeddingService episodicMemoryEmbeddingService;

    /**
     * List memory references attached to a message, enriched with memory-type metadata
     * so the frontend can group semantic vs episodic hits in the "Memory" popup.
     * <p>
     * Looks up each {@code embeddingId} against the semantic store first, then any
     * still-unresolved ids against the episodic store. Structured fields (event_type,
     * importance, created_at) and the memory-type tag are read directly from the
     * vector store's metadata — no relational table is consulted.
     * <p>
     * 列出消息引用的记忆，并附带 memory type 元数据，让前端能在"记忆"弹窗中分组展示
     * semantic / episodic。先在语义向量库反查，未命中的再去情景向量库反查；结构化字段
     * 和类型标记直接从向量库 metadata 读取，不再依赖关系表。
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

        // First lookup: semantic store (also covers legacy rows without memory_type tag).
        // 一查 semantic store（同时覆盖无 memory_type 标签的历史数据）。
        List<KbItemEmbeddingDto> semanticHits = characterMemoryEmbeddingService.listByEmbeddingIds(embeddingIds);
        Map<String, KbItemEmbeddingDto> byId = new HashMap<>();
        for (KbItemEmbeddingDto hit : semanticHits) {
            byId.put(hit.getEmbeddingId(), hit);
        }

        // Second lookup: any ids not found in the semantic store must be in the
        // episodic store (physically isolated since the split).
        // 二查 episodic store：未命中的 id 应来自物理隔离后的情景表。
        List<String> missingIds = new ArrayList<>();
        for (String id : embeddingIds) {
            if (!byId.containsKey(id)) {
                missingIds.add(id);
            }
        }
        if (!missingIds.isEmpty()) {
            List<KbItemEmbeddingDto> episodicHits = episodicMemoryEmbeddingService.listByEmbeddingIds(missingIds);
            for (KbItemEmbeddingDto hit : episodicHits) {
                byId.put(hit.getEmbeddingId(), hit);
            }
        }

        // Preserve original ordering from the relational reference table.
        // 按关联表原始顺序输出。
        List<KbItemEmbeddingDto> orderedHits = new ArrayList<>();
        for (String id : embeddingIds) {
            KbItemEmbeddingDto hit = byId.get(id);
            if (hit != null) {
                orderedHits.add(hit);
            }
        }

        List<RefEmbeddingDto> dtos = EmbeddingUtil.itemToRefEmbeddingDto(orderedHits);
        for (int i = 0; i < dtos.size(); i++) {
            RefEmbeddingDto dto = dtos.get(i);
            JsonNode metadata = orderedHits.get(i).getMetadata();
            applyMemoryTypeAndFields(dto, metadata);
        }
        return dtos;
    }

    /**
     * Read memory_type and episodic structured fields off the vector store metadata.
     * Legacy rows without a {@code memory_type} tag are treated as semantic.
     * <p>
     * 从向量库 metadata 读取 memory_type 和情景记忆的结构化字段。无 memory_type 标签的
     * 历史数据默认归为 semantic。
     */
    private void applyMemoryTypeAndFields(RefEmbeddingDto dto, JsonNode metadata) {
        String memoryType = readString(metadata, MEMORY_TYPE);
        if (AdiConstant.MemoryType.EPISODIC.equals(memoryType)) {
            dto.setMemoryType(AdiConstant.MemoryType.EPISODIC);
            dto.setEventType(readString(metadata, EVENT_TYPE));
            dto.setCreatedAt(readString(metadata, CREATED_AT));
            dto.setImportance(readInt(metadata, IMPORTANCE));
        } else {
            dto.setMemoryType(AdiConstant.MemoryType.SEMANTIC);
        }
    }

    private static String readString(JsonNode metadata, String key) {
        if (metadata == null) {
            return null;
        }
        JsonNode value = metadata.get(key);
        return value == null || value.isNull() ? null : value.asText();
    }

    /**
     * Read an int-valued metadata field defensively. The Neo4j backend serializes
     * all metadata values as strings (see {@code Neo4jEmbeddingUtils.toEmbeddingMatch}),
     * so {@code IntNode} cannot be assumed even though pgvector stores integers
     * natively. Returns {@code null} on missing / unparseable values so the UI
     * field stays empty instead of defaulting to 0.
     * <p>
     * 防御性读取整数 metadata 字段：Neo4j 后端把所有 metadata 值序列化为字符串，
     * 因此即使 pgvector 原生存整数也不能假设是 IntNode。缺失或无法解析时返回 null，
     * 让 UI 字段为空而非默认 0。
     */
    private static Integer readInt(JsonNode metadata, String key) {
        if (metadata == null) {
            return null;
        }
        JsonNode value = metadata.get(key);
        if (value == null || value.isNull()) {
            return null;
        }
        if (value.isNumber()) {
            return value.asInt();
        }
        try {
            return Integer.parseInt(value.asText());
        } catch (NumberFormatException e) {
            log.warn("Unparseable {} in metadata: {}", key, value);
            return null;
        }
    }
}

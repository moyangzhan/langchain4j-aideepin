package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.fasterxml.jackson.databind.JsonNode;
import com.moyz.adi.common.dto.KbItemEmbeddingDto;
import com.moyz.adi.common.dto.RefEmbeddingDto;
import com.moyz.adi.common.entity.CharacterMessageRefMemoryEmbedding;
import com.moyz.adi.common.enums.MemoryType;
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
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.moyz.adi.common.cosntant.AdiConstant.MetadataKey.CREATE_TIME;
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
     * Each ref row carries a {@code memory_type} routing code; rows are bucketed by
     * code and each bucket is resolved against its dedicated physical vector store
     * in a single batch lookup — no fallback probing across stores.
     * <p>
     * 列出消息引用的记忆。每条 ref 行带 memory_type 路由码，按 enum 分桶后各自走对应的
     * 物理向量库一次性反查；不再做"先 semantic 后 episodic"的盲查 fallback。
     */
    public List<RefEmbeddingDto> listRefEmbeddings(String uuid) {
        List<CharacterMessageRefMemoryEmbedding> recordReferences = this.getBaseMapper().listByMsgUuid(uuid);
        if (CollectionUtils.isEmpty(recordReferences)) {
            return Collections.emptyList();
        }

        // Bucket by routing code so each physical store is queried at most once.
        // 按路由码分桶，每个物理向量库最多查一次。
        Map<MemoryType, List<String>> byType = new EnumMap<>(MemoryType.class);
        List<String> orderedIds = new ArrayList<>(recordReferences.size());
        for (CharacterMessageRefMemoryEmbedding ref : recordReferences) {
            if (ref.getEmbeddingId() == null || ref.getEmbeddingId().isEmpty()) {
                continue;
            }
            MemoryType type = ref.getMemoryType();
            byType.computeIfAbsent(type, k -> new ArrayList<>()).add(ref.getEmbeddingId());
            orderedIds.add(ref.getEmbeddingId());
        }
        if (orderedIds.isEmpty()) {
            return Collections.emptyList();
        }

        Map<String, KbItemEmbeddingDto> byId = new HashMap<>();
        for (Map.Entry<MemoryType, List<String>> entry : byType.entrySet()) {
            List<String> ids = entry.getValue();
            if (ids.isEmpty()) {
                continue;
            }
            List<KbItemEmbeddingDto> hits = switch (entry.getKey()) {
                case SEMANTIC -> characterMemoryEmbeddingService.listByEmbeddingIds(ids);
                case EPISODIC -> episodicMemoryEmbeddingService.listByEmbeddingIds(ids);
                case PROCEDURAL -> {
                    // Procedural store is not implemented yet — log and skip so a stray
                    // routing code can't crash the whole memory popup.
                    // 程序性记忆尚未实现，忽略以保护现网调用。
                    log.warn("Procedural memory store not implemented, skipping {} refs", ids.size());
                    yield Collections.emptyList();
                }
            };
            for (KbItemEmbeddingDto hit : hits) {
                byId.put(hit.getEmbeddingId(), hit);
            }
        }

        // Preserve original ordering from the relational reference table.
        // 按关联表原始顺序输出。
        List<KbItemEmbeddingDto> orderedHits = new ArrayList<>();
        for (String id : orderedIds) {
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
        if (MemoryType.EPISODIC.getDesc().equals(memoryType)) {
            dto.setMemoryType(MemoryType.EPISODIC.getDesc());
            dto.setEventType(readString(metadata, EVENT_TYPE));
            dto.setCreateTime(readString(metadata, CREATE_TIME));
            dto.setImportance(readInt(metadata, IMPORTANCE));
        } else {
            dto.setMemoryType(MemoryType.SEMANTIC.getDesc());
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

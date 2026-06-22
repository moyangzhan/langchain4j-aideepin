package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.entity.CharacterEpisodicMemory;
import com.moyz.adi.common.mapper.CharacterEpisodicMemoryMapper;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.List;

/**
 * CRUD and timeline query service for episodic memory.
 * <p>
 * 情景记忆的 CRUD 和时间轴查询服务。
 */
@Slf4j
@Service
public class CharacterEpisodicMemoryService extends ServiceImpl<CharacterEpisodicMemoryMapper, CharacterEpisodicMemory> {

    /**
     * List recent episodic memories for a character, time-desc.
     * <p>
     * 按时间倒序拉取角色最近的情景记忆。
     */
    public List<CharacterEpisodicMemory> listRecentByCharacter(Long characterId, Long userId, int limit) {
        return baseMapper.listRecent(characterId, userId, limit);
    }

    /**
     * Bulk lookup episodic rows by embedding ids — used for joining vector retrieval back to
     * structured metadata (created_at / event_type / importance).
     * <p>
     * 通过 embedding_id 批量回查结构化字段，用于召回侧加权排序。
     */
    public List<CharacterEpisodicMemory> listByEmbeddingIds(List<String> embeddingIds) {
        if (CollectionUtils.isEmpty(embeddingIds)) {
            return Collections.emptyList();
        }
        return baseMapper.listByEmbeddingIds(embeddingIds);
    }

    /**
     * Idempotency check: has this source message already produced episodic records?
     * <p>
     * 幂等校验：当前 source_msg_id 是否已经产出过 episodic 记录。
     */
    public boolean existsBySourceMsgId(Long sourceMsgId) {
        if (sourceMsgId == null) {
            return false;
        }
        return baseMapper.countBySourceMsgId(sourceMsgId) > 0;
    }
}
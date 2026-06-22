package com.moyz.adi.common.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.moyz.adi.common.entity.CharacterEpisodicMemory;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * Mapper for episodic memory CRUD and timeline queries.
 * <p>
 * 情景记忆 Mapper，提供 CRUD 和时间轴查询。
 */
@Mapper
public interface CharacterEpisodicMemoryMapper extends BaseMapper<CharacterEpisodicMemory> {

    /**
     * 按角色和用户查询最近的 episodic 记忆（时间倒序）。
     * <p>
     * List recent episodic memories for a character, ordered by recency.
     */
    List<CharacterEpisodicMemory> listRecent(@Param("characterId") Long characterId,
                                              @Param("userId") Long userId,
                                              @Param("limit") int limit);

    /**
     * 按 embedding_id 列表批量查询。
     * <p>
     * Batch query by embedding IDs.
     */
    List<CharacterEpisodicMemory> listByEmbeddingIds(@Param("embeddingIds") List<String> embeddingIds);

    /**
     * 按 source_msg_id 去重判断是否已存在。
     * <p>
     * Check if an episodic record already exists for a given source message (idempotency).
     */
    int countBySourceMsgId(@Param("sourceMsgId") Long sourceMsgId);
}
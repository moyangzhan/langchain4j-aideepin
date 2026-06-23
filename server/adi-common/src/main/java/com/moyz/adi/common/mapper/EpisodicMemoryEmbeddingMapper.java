package com.moyz.adi.common.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.moyz.adi.common.entity.EpisodicMemoryEmbedding;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface EpisodicMemoryEmbeddingMapper extends BaseMapper<EpisodicMemoryEmbedding> {

    /**
     * Timeline retrieval: list the most recent {@code limit} episodic events for a
     * character, ordered by the {@code created_at} value stored in metadata.
     * <p>
     * 时间轴检索：按 metadata 中的 {@code created_at} 倒序，返回某角色最近 {@code limit} 条事件。
     */
    List<EpisodicMemoryEmbedding> listRecentByCharacter(@Param("characterId") Long characterId,
                                                       @Param("limit") int limit);
}


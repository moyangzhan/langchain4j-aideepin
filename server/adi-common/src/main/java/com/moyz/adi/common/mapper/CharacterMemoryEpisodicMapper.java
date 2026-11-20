package com.moyz.adi.common.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.moyz.adi.common.entity.CharacterMemoryEpisodic;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface CharacterMemoryEpisodicMapper extends BaseMapper<CharacterMemoryEpisodic> {

    /**
     * 按记忆uuid批量累加命中次数（软删行不计）
     * <p>
     * Increment hit counts by memory uuid in batch (soft-deleted rows excluded).
     */
    void incrementHitCountByUuids(@Param("uuids") List<String> uuids);
}

package com.moyz.adi.common.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.entity.CharacterMemoryEmbedding;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface CharacterMemoryEmbeddingMapper extends BaseMapper<CharacterMemoryEmbedding> {

    Page<CharacterMemoryEmbedding> selectByMsgId(Page<CharacterMemoryEmbedding> page, @Param("msgId") Long msgId, @Param("tableSuffix") String tableSuffix);

    boolean deleteByMsgId(@Param("msgId") Long msgId, @Param("tableSuffix") String tableSuffix);

    Integer countByCharacterId(@Param("characterId") Long characterId, @Param("tableSuffix") String tableSuffix);

    int incrementHitCount(@Param("embeddingIds") List<String> embeddingIds, @Param("tableSuffix") String tableSuffix);
}

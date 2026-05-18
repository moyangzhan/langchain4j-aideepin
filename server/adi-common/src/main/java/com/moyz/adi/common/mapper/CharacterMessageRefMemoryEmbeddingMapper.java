package com.moyz.adi.common.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.moyz.adi.common.entity.CharacterMessageRefMemoryEmbedding;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface CharacterMessageRefMemoryEmbeddingMapper extends BaseMapper<CharacterMessageRefMemoryEmbedding> {
    List<CharacterMessageRefMemoryEmbedding> listByMsgUuid(@Param("uuid") String msgUuid);
}

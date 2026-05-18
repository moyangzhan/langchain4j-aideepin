package com.moyz.adi.common.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.moyz.adi.common.entity.CharacterMessageRefEmbedding;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface CharacterMessageRefEmbeddingMapper extends BaseMapper<CharacterMessageRefEmbedding> {
    List<CharacterMessageRefEmbedding> listByMsgUuid(@Param("uuid") String uuid);
}

package com.moyz.adi.common.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.moyz.adi.common.entity.CharacterMessageRefGraph;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface CharacterMessageRefGraphMapper extends BaseMapper<CharacterMessageRefGraph> {
    List<CharacterMessageRefGraph> listByMsgUuid(@Param("uuid") String uuid);
}

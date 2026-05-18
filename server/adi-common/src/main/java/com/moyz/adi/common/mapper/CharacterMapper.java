package com.moyz.adi.common.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.moyz.adi.common.entity.Character;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.time.LocalDateTime;

@Mapper
public interface CharacterMapper extends BaseMapper<Character> {
    Integer countCreatedByTimePeriod(@Param("beginTime") LocalDateTime beginTime, @Param("endTime") LocalDateTime endTime);

    Integer countAllCreated();
}

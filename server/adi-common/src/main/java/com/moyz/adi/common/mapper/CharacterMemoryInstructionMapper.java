package com.moyz.adi.common.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.moyz.adi.common.entity.CharacterMemoryInstruction;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

@Mapper
public interface CharacterMemoryInstructionMapper extends BaseMapper<CharacterMemoryInstruction> {

    /**
     * 按记忆uuid upsert 内容（UPDATE 合并路径；行不存在时补插关系行，软删行复活）
     * <p>
     * Upsert content by memory uuid (merge UPDATE path; inserts the relational row
     * when absent, revives a soft-deleted row).
     */
    void upsertContent(@Param("uuid") String uuid,
                       @Param("characterId") Long characterId,
                       @Param("content") String content,
                       @Param("userId") Long userId);
}

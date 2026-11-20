package com.moyz.adi.common.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.moyz.adi.common.entity.CharacterMemorySemantic;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface CharacterMemorySemanticMapper extends BaseMapper<CharacterMemorySemantic> {

    /**
     * 按记忆uuid upsert 内容（UPDATE 合并路径；行不存在时补插关系行）
     * <p>
     * Upsert content by memory uuid (merge UPDATE path; inserts the relational row
     * when absent).
     */
    void upsertContent(@Param("uuid") String uuid,
                       @Param("characterId") Long characterId,
                       @Param("content") String content,
                       @Param("userId") Long userId);

    /**
     * 按记忆uuid批量累加命中次数（软删行不计）
     * <p>
     * Increment hit counts by memory uuid in batch (soft-deleted rows excluded).
     */
    void incrementHitCountByUuids(@Param("uuids") List<String> uuids);
}

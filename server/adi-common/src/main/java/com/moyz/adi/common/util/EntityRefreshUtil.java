package com.moyz.adi.common.util;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.moyz.adi.common.entity.BaseEntity;

/**
 * 实体回查工具：插入后按主键重新读取一行，让由数据库维护的列
 * （create_time / update_time / is_deleted）回填到内存对象，避免调用方拿到这些字段的 null 值。
 * <p>
 * Entity refresh utility: re-reads the row by primary key after insert so database-managed columns
 * (create_time / update_time / is_deleted) are populated in memory, sparing callers from seeing
 * null values for these fields.
 */
public final class EntityRefreshUtil {

    private EntityRefreshUtil() {
    }

    /**
     * 按主键重新查询并返回该实体。若 id 为空或查询不到，则原样返回入参实体。
     *
     * @param mapper 实体对应的 Mapper
     * @param entity 刚插入的实体（须已带主键）
     * @param <T>    实体类型
     * @return 回查到的实体；查不到时返回入参 entity
     */
    public static <T extends BaseEntity> T refresh(BaseMapper<T> mapper, T entity) {
        if (entity == null || entity.getId() == null) {
            return entity;
        }
        T fresh = mapper.selectById(entity.getId());
        return fresh != null ? fresh : entity;
    }
}

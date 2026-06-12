package com.moyz.adi.common.config;

import com.baomidou.mybatisplus.core.handlers.MetaObjectHandler;
import lombok.extern.slf4j.Slf4j;
import org.apache.ibatis.reflection.MetaObject;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;

/**
 * MyBatis Plus 自动填充处理器：在 insert / update 时把 createTime / updateTime / isDeleted
 * 写回 entity 实例，避免 BeanUtils.copyProperties 拷贝出 null。
 * <p>
 * 注意：这只影响 entity 在 Java 内存中的值与 SQL 中携带的字段值。数据库实际持久化的
 * update_time 由 BEFORE UPDATE 触发器（{@code update_modified_column()}）覆写，所以
 * 内存里的 updateTime 与 DB 落库值在毫秒级可能不一致 —— 这是预期行为：DB 是 update_time 的真相之源。
 * <p>
 * MyBatis Plus auto-fill handler. On insert / update, populates createTime / updateTime /
 * isDeleted on the entity instance so subsequent BeanUtils.copyProperties calls receive non-null
 * values.
 * <p>
 * Note: this only affects the in-memory entity value and the column value carried in the SQL.
 * The persisted update_time is overwritten by the BEFORE UPDATE trigger
 * ({@code update_modified_column()}), so the in-memory updateTime may differ from the DB value
 * by a few milliseconds — that is intentional: the DB is the source of truth for update_time.
 */
@Slf4j
@Component
public class MybatisPlusMetaObjectHandler implements MetaObjectHandler {

    @Override
    public void insertFill(MetaObject metaObject) {
        LocalDateTime now = LocalDateTime.now();
        this.strictInsertFill(metaObject, "createTime", LocalDateTime.class, now);
        this.strictInsertFill(metaObject, "updateTime", LocalDateTime.class, now);
        this.strictInsertFill(metaObject, "isDeleted", Boolean.class, false);
    }

    @Override
    public void updateFill(MetaObject metaObject) {
        // 仅把 updateTime 写回 entity 内存对象，便于调用方拷贝 DTO；DB 落库值仍由触发器维护。
        // <p>
        // Only populate updateTime on the in-memory entity for caller convenience; the persisted
        // value remains under the database trigger's control.
        this.strictUpdateFill(metaObject, "updateTime", LocalDateTime.class, LocalDateTime.now());
    }
}

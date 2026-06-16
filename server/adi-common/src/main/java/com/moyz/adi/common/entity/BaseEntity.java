package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import lombok.Data;

import java.io.Serializable;
import java.time.LocalDateTime;

/**
 * 所有实体的基类。create_time / update_time / is_deleted 不再走 MybatisPlus 自动填充：
 * 三列均由数据库维护（DDL 提供 default，update_time 由 BEFORE UPDATE 触发器覆写），
 * Java 端只负责回查读取，避免内存值与落库值产生偏差。
 * <p>
 * Base class for all entities. create_time / update_time / is_deleted no longer use MybatisPlus
 * auto-fill: all three are owned by the database (DDL provides defaults, update_time is overwritten
 * by a BEFORE UPDATE trigger). The Java side only re-reads them, keeping memory in sync with the DB.
 */
@Data
public class BaseEntity implements Serializable {

    private static final long serialVersionUID = 1L;

    @TableId(type = IdType.AUTO)
    private Long id;

    @TableField(value = "create_time")
    private LocalDateTime createTime;

    @TableField(value = "update_time")
    private LocalDateTime updateTime;

    @TableField(value = "is_deleted")
    private Boolean isDeleted;
}

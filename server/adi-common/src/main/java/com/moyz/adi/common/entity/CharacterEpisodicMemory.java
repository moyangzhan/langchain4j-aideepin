package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;
import java.time.LocalDateTime;

/**
 * Episodic memory entity — append-only events extracted from conversation turns.
 * One row per extracted episodic event, with timeline-based retrieval capabilities.
 * <p>
 * 情景记忆实体 —— 从对话中提取的 append-only 事件，支持时间轴检索。
 */
@Data
@TableName("adi_character_episodic_memory")
@Schema(title = "情景记忆实体 | Episodic Memory Entity", description = "角色情景记忆表 | Character Episodic Memory Table")
public class CharacterEpisodicMemory implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    @TableId(type = IdType.AUTO)
    private Long id;

    @Schema(title = "UUID")
    @TableField("uuid")
    private String uuid;

    @Schema(title = "角色ID | Character ID")
    @TableField("character_id")
    private Long characterId;

    @Schema(title = "用户ID | User ID")
    @TableField("user_id")
    private Long userId;

    @Schema(title = "事件摘要 | Event summary")
    @TableField("summary")
    private String summary;

    @Schema(title = "源消息ID | Source message ID")
    @TableField("source_msg_id")
    private Long sourceMsgId;

    @Schema(title = "事件类型 | Event type (travel/health/work/general/...)")
    @TableField("event_type")
    private String eventType;

    @Schema(title = "重要性 (1-5) | Importance (1-5)", description = "LLM 判定，1 最低 5 最高")
    @TableField("importance")
    private Integer importance;

    @Schema(title = "向量库 embedding ID | Embedding store ID")
    @TableField("embedding_id")
    private String embeddingId;

    @Schema(title = "创建时间 | Created time")
    @TableField("created_at")
    private LocalDateTime createdAt;

    @Schema(title = "最后访问时间 | Last accessed time")
    @TableField("last_accessed_at")
    private LocalDateTime lastAccessedAt;

    @Schema(title = "命中次数 | Hit count")
    @TableField("hit_count")
    private Integer hitCount;

    @Schema(title = "是否活跃 | Active flag")
    @TableField("is_active")
    private Boolean isActive;

    @Schema(title = "软删除标记 | Soft delete flag")
    @TableField("is_deleted")
    private Boolean isDeleted;
}
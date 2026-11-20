package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.FieldStrategy;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 情景记忆关系行——记忆内容与结构化字段的唯一事实源。向量表主键 = 本行 uuid 的
 * canonical 36 位形（补横线），按 uuid 联接回表。
 * <p>
 * Episodic memory relational row - single source of truth for content and
 * structured fields. The vector table primary key equals this row's uuid in
 * canonical 36-char dashed form; hydration joins back on uuid.
 */
@EqualsAndHashCode(callSuper = true)
@Data
@TableName("adi_character_memory_episodic")
@Schema(title = "角色-情景记忆实体 | Character Episodic Memory Entity", description = "角色情景记忆表 | Character Episodic Memory Table")
public class CharacterMemoryEpisodic extends BaseEntity {

    @Schema(title = "记忆uuid(向量表主键为其canonical形) | Memory UUID")
    @TableField("uuid")
    private String uuid;

    @Schema(title = "所属角色id | Character ID")
    @TableField("character_id")
    private Long characterId;

    @Schema(title = "记忆内容 | Content")
    @TableField("content")
    private String content;

    @Schema(title = "事件类型码 | Event Type")
    @TableField("event_type")
    private String eventType;

    @Schema(title = "重要性(1-5) | Importance")
    @TableField("importance")
    private Integer importance;

    @Schema(title = "来源消息id(-1缺省) | Source Message ID")
    @TableField("source_msg_id")
    private Long sourceMsgId;

    @Schema(title = "所有者用户id(-1缺省) | Owner User ID")
    @TableField("user_id")
    private Long userId;

    @Schema(title = "命中次数 | Hit Count")
    @TableField("hit_count")
    private Integer hitCount;

    @Schema(title = "字符数 | Word Count (auto-computed by PostgreSQL)")
    @TableField(value = "word_count", insertStrategy = FieldStrategy.NEVER, updateStrategy = FieldStrategy.NEVER)
    private Integer wordCount;
}

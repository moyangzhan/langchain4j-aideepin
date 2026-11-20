package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.FieldStrategy;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 语义记忆关系行——记忆内容的唯一事实源，一条事实一行。向量表主键 = 本行 uuid 的
 * canonical 36 位形（补横线），按 uuid 联接回表。
 * <p>
 * Semantic memory relational row - single source of truth for content, one fact
 * per row. The vector table primary key equals this row's uuid in canonical
 * 36-char dashed form; hydration joins back on uuid.
 */
@EqualsAndHashCode(callSuper = true)
@Data
@TableName("adi_character_memory_semantic")
@Schema(title = "角色-语义记忆实体 | Character Semantic Memory Entity", description = "角色语义记忆表 | Character Semantic Memory Table")
public class CharacterMemorySemantic extends BaseEntity {

    @Schema(title = "记忆uuid(向量表主键为其canonical形) | Memory UUID")
    @TableField("uuid")
    private String uuid;

    @Schema(title = "所属角色id | Character ID")
    @TableField("character_id")
    private Long characterId;

    @Schema(title = "记忆内容 | Content")
    @TableField("content")
    private String content;

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

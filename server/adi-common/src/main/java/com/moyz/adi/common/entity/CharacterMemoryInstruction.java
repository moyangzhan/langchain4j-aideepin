package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.FieldStrategy;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 指令记忆关系行——记忆内容的唯一事实源，一条行为规则一行。向量表主键 = 本行 uuid 的
 * canonical 36 位形（补横线），按 uuid 联接回表。
 * <p>
 * Instruction memory relational row - single source of truth for content, one
 * behaviour rule per row. The vector table primary key equals this row's uuid in
 * canonical 36-char dashed form; hydration joins back on uuid.
 */
@EqualsAndHashCode(callSuper = true)
@Data
@TableName("adi_character_memory_instruction")
@Schema(title = "角色-指令记忆实体 | Character Instruction Memory Entity", description = "角色指令记忆表 | Character Instruction Memory Table")
public class CharacterMemoryInstruction extends BaseEntity {

    @Schema(title = "记忆uuid(向量表主键为其canonical形) | Memory UUID")
    @TableField("uuid")
    private String uuid;

    @Schema(title = "所属角色id | Character ID")
    @TableField("character_id")
    private Long characterId;

    @Schema(title = "记忆内容 | Content")
    @TableField("content")
    private String content;

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

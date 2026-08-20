package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.FieldStrategy;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 父子模式的子块。子块原文被向量化；父段存于 adi_document_segment（不向量化），
 * 命中子块后上卷到父段内容返回。
 */
@EqualsAndHashCode(callSuper = true)
@Data
@TableName("adi_document_segment_child_chunk")
@Schema(title = "文档分段-子块实体 | Document Segment Child Chunk Entity", description = "文档分段子块表 | Document Segment Child Chunk Table")
public class DocumentSegmentChildChunk extends BaseEntity {

    @Schema(title = "uuid")
    @TableField("uuid")
    private String uuid;

    @Schema(title = "所属知识库uuid | Knowledge Base UUID")
    @TableField("kb_uuid")
    private String kbUuid;

    @Schema(title = "所属文档uuid | Document UUID")
    @TableField("doc_uuid")
    private String docUuid;

    @Schema(title = "父分段id | Parent Segment ID")
    @TableField("parent_segment_id")
    private Long parentSegmentId;

    @Schema(title = "子块在其父分段内的顺序 | Position within the parent segment")
    @TableField("position")
    private Integer position;

    @Schema(title = "子块原文(被向量化) | Child chunk original text")
    @TableField("content")
    private String content;

    @Schema(title = "字符数 | Word Count (auto-computed by PostgreSQL)")
    @TableField(value = "word_count", insertStrategy = FieldStrategy.NEVER, updateStrategy = FieldStrategy.NEVER)
    private Integer wordCount;

    @Schema(title = "命中次数 | Hit Count")
    @TableField("hit_count")
    private Integer hitCount;

    @Schema(title = "向量库条目id | Vector store entry id")
    @TableField("embedding_id")
    private String embeddingId;
}

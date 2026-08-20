package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.FieldStrategy;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 问答模式的问题。问题原文被向量化；答案存于 adi_document_segment（不向量化），
 * 多个问题可通过 answer_segment_id 关联到同一个答案。
 * 答案版本、引用来源等 QA 专属扩展将来加在本表，不动主表。
 */
@EqualsAndHashCode(callSuper = true)
@Data
@TableName("adi_document_segment_question")
@Schema(title = "文档分段-问题实体 | Document Segment Question Entity", description = "文档分段问题表 | Document Segment Question Table")
public class DocumentSegmentQuestion extends BaseEntity {

    @Schema(title = "uuid")
    @TableField("uuid")
    private String uuid;

    @Schema(title = "所属知识库uuid | Knowledge Base UUID")
    @TableField("kb_uuid")
    private String kbUuid;

    @Schema(title = "所属文档uuid | Document UUID")
    @TableField("doc_uuid")
    private String docUuid;

    @Schema(title = "答案分段id | Answer Segment ID")
    @TableField("answer_segment_id")
    private Long answerSegmentId;

    @Schema(title = "问题在其答案分段内的顺序 | Position within the answer segment")
    @TableField("position")
    private Integer position;

    @Schema(title = "问题原文(被向量化) | Question original text")
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

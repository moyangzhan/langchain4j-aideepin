package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.FieldStrategy;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.time.LocalDateTime;

/**
 * 文档分段（text 分段 / qa 答案 / parent_child 父段）——分段内容与元数据的唯一事实源。
 * text 模式下行自身被向量化（embedding_id 非空）；qa 与 parent_child 模式下行不被向量化，
 * 被向量化的是子表中的问题/子块，命中后上卷到本表行。
 */
@EqualsAndHashCode(callSuper = true)
@Data
@TableName("adi_document_segment")
@Schema(title = "文档分段实体 | Document Segment Entity", description = "文档分段表 | Document Segment Table")
public class DocumentSegment extends BaseEntity {

    @Schema(title = "uuid(图谱化时同时作为顶点/边的textSegmentId) | uuid")
    @TableField("uuid")
    private String uuid;

    @Schema(title = "所属知识库uuid | Knowledge Base UUID")
    @TableField("kb_uuid")
    private String kbUuid;

    @Schema(title = "所属文档uuid | Document UUID")
    @TableField("doc_uuid")
    private String docUuid;

    @Schema(title = "段在文档中的顺序 | Position in document")
    @TableField("position")
    private Integer position;

    @Schema(title = "内容: text=分段文本/qa=答案/parent_child=父段 | Content")
    @TableField("content")
    private String content;

    @Schema(title = "字符数 | Word Count (auto-computed by PostgreSQL)")
    @TableField(value = "word_count", insertStrategy = FieldStrategy.NEVER, updateStrategy = FieldStrategy.NEVER)
    private Integer wordCount;

    @Schema(title = "命中次数: text=直接命中; qa/parent_child=由关联问题/子块命中传导 | Hit Count")
    @TableField("hit_count")
    private Integer hitCount;

    @Schema(title = "向量库条目id,仅text模式非空 | Vector store entry id")
    @TableField("embedding_id")
    private String embeddingId;

    @Schema(title = "来源: doc/manual/annotation(预留) | Source")
    @TableField("source")
    private String source;

    @Schema(title = "是否启用 | Is Enabled")
    @TableField("is_enabled")
    private Boolean isEnabled;

    @Schema(title = "启用/停用变更时间 | Enabled Status Change Time")
    @TableField("enabled_change_time")
    private LocalDateTime enabledChangeTime;
}

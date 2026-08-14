package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.FieldStrategy;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import com.moyz.adi.common.enums.EmbeddingStatusEnum;
import com.moyz.adi.common.enums.GraphicalStatusEnum;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.time.LocalDateTime;

@Data
@TableName("adi_knowledge_base_item")
@Schema(title = "知识库文档实体 | Knowledge Base Document Entity", description = "知识库文档表 | Knowledge Base Document Table")
public class KnowledgeBaseItem extends BaseEntity {

    @Schema(title = "知识库id | Knowledge Base ID")
    @TableField("kb_id")
    private Long kbId;

    @Schema(title = "知识库uuid | Knowledge Base UUID")
    @TableField("kb_uuid")
    private String kbUuid;

    @Schema(title = "名称 | Name")
    @TableField("source_file_id")
    private Long sourceFileId;

    @Schema(title = "uuid")
    @TableField("uuid")
    private String uuid;

    @Schema(title = "标题 | Title")
    @TableField("title")
    private String title;

    @Schema(title = "内容摘要 | Content Summary")
    @TableField("brief")
    private String brief;

    @Schema(title = "内容 | Content")
    @TableField("remark")
    private String remark;

    @Schema(title = "向量化状态 | Embedding Status")
    @TableField("embedding_status")
    private EmbeddingStatusEnum embeddingStatus;

    @Schema(title = "向量化状态变更时间点 | Embedding Status Change Time")
    @TableField("embedding_status_change_time")
    private LocalDateTime embeddingStatusChangeTime;

    @Schema(title = "图谱化状态 | Graphical Status")
    @TableField("graphical_status")
    private GraphicalStatusEnum graphicalStatus;

    @Schema(title = "图谱化状态变更时间点 | Graphical Status Change Time")
    @TableField("graphical_status_change_time")
    private LocalDateTime graphicalStatusChangeTime;

    @Schema(title = "向量命中次数 | Embedding Hit Count")
    @TableField("embedding_hit_count")
    private Integer embeddingHitCount;

    @Schema(title = "图谱命中次数 | Graph Hit Count")
    @TableField("graph_hit_count")
    private Integer graphHitCount;

    @Schema(title = "字符数 | Word Count (auto-computed by PostgreSQL)")
    @TableField(value = "word_count", insertStrategy = FieldStrategy.NEVER, updateStrategy = FieldStrategy.NEVER)
    private Integer wordCount;

    @Schema(title = "是否启用 | Is Enabled")
    @TableField("is_enabled")
    private Boolean isEnabled;

    @Schema(title = "启用/停用变更时间 | Enabled Status Change Time")
    @TableField("enabled_change_time")
    private LocalDateTime enabledChangeTime;
}

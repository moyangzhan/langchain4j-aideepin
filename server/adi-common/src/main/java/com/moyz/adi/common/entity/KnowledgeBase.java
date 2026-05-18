package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

@EqualsAndHashCode(callSuper = true)
@Data
@TableName("adi_knowledge_base")
@Schema(title = "知识库实体 | Knowledge Base Entity", description = "知识库表 | Knowledge Base Table")
public class KnowledgeBase extends BaseEntity {

    @Schema(title = "uuid")
    @TableField("uuid")
    private String uuid;

    @Schema(title = "名称 | Name")
    @TableField("title")
    private String title;

    @Schema(title = "描述 | Description")
    @TableField("remark")
    private String remark;

    @Schema(title = "是否公开 | Is Public")
    @TableField("is_public")
    private Boolean isPublic;

    @Schema(title = "是否严格模式 | Is Strict Mode")
    @TableField("is_strict")
    private Boolean isStrict;

    @Schema(title = "点赞数 | Star Count")
    @TableField("star_count")
    private Integer starCount;

    @Schema(title = "知识点数量 | Knowledge Item Count")
    @TableField("item_count")
    private Integer itemCount;

    @Schema(title = "向量数 | Embedding Count")
    @TableField("embedding_count")
    private Integer embeddingCount;

    @Schema(title = "所属人uuid | Owner UUID")
    @TableField("owner_uuid")
    private String ownerUuid;

    @Schema(title = "所属人id | Owner ID")
    @TableField("owner_id")
    private Long ownerId;

    @Schema(title = "所属人名称 | Owner Name")
    @TableField("owner_name")
    private String ownerName;

    @Schema(title = "文档切割时重叠数量(按token来计) | Document Chunking Overlap Count (by Token)")
    @TableField("ingest_max_overlap")
    private Integer ingestMaxOverlap;

    @Schema(title = "索引(图谱化)文档时使用的LLM,如不指定的话则使用第1个可用的LLM | LLM Used for Indexing (Graph) - Defaults to First Available")
    @TableField("ingest_model_name")
    private String ingestModelName;

    @Schema(title = "索引(图谱化)文档时使用的LLM,如不指定的话则使用第1个可用的LLM | LLM Used for Indexing (Graph) - Defaults to First Available")
    @TableField("ingest_model_id")
    private Long ingestModelId;

    @Schema(title = "token数量估计器,默认使用OpenAiTokenizer | Token Estimator (Default: OpenAiTokenizer)")
    @TableField("ingest_token_estimator")
    private String ingestTokenEstimator;

    @Schema(title = "文档召回最大数量 | Max Document Recall Count")
    @TableField("retrieve_max_results")
    private Integer retrieveMaxResults;

    @Schema(title = "文档召回最小分数 | Min Document Recall Score")
    @TableField("retrieve_min_score")
    private Double retrieveMinScore;

    @Schema(title = "请求LLM时的temperature | LLM Temperature")
    @TableField("query_llm_temperature")
    private Double queryLlmTemperature;

    @Schema(title = "请求LLM时的系统提示词 | System Prompt for LLM")
    @TableField("query_system_message")
    private String querySystemMessage;

    @Schema(title = "OpenAPI密钥 | OpenAPI API Key")
    @TableField("api_key")
    private String apiKey;
}

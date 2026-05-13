package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.apache.ibatis.type.JdbcType;

@Data
@TableName("adi_knowledge_base_qa")
@Schema(title = "知识库问答记录实体 | Knowledge Base QA Record Entity", description = "知识库问答记录表 | Knowledge Base QA Record Table")
public class KnowledgeBaseQa extends BaseEntity {

    @Schema(title = "uuid")
    @TableField(value = "uuid", jdbcType = JdbcType.VARCHAR)
    private String uuid;

    @Schema(title = "知识库id | Knowledge Base ID")
    @TableField("kb_id")
    private Long kbId;

    @Schema(title = "知识库uuid | Knowledge Base UUID")
    @TableField("kb_uuid")
    private String kbUuid;

    @Schema(title = "来源文档id,以逗号隔开 | Source Document IDs (Comma Separated)")
    @TableField("source_file_ids")
    private String sourceFileIds;

    @Schema(title = "问题 | Question")
    @TableField("question")
    private String question;

    @Schema(title = "最终提供给LLM的提示词 | Final Prompt Provided to LLM")
    @TableField("prompt")
    private String prompt;

    @Schema(title = "提供给LLM的提示词所消耗的token数量 | Token Count Consumed by Prompt to LLM")
    @TableField("prompt_tokens")
    private Integer promptTokens;

    @Schema(title = "答案 | Answer")
    @TableField("answer")
    private String answer;

    @Schema(title = "答案消耗的token | Answer Token Count")
    @TableField("answer_tokens")
    private Integer answerTokens;

    @Schema(title = "提问用户id | Question User ID")
    @TableField("user_id")
    private Long userId;

    @Schema(title = "adi_ai_model id")
    @TableField("ai_model_id")
    private Long aiModelId;
}

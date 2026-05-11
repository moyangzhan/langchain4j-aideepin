package com.moyz.adi.common.dto;

import com.baomidou.mybatisplus.annotation.TableField;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Data
public class KbQaDto {
    @Schema(title = "uuid")
    private String uuid;

    @Schema(title = "知识库uuid | Knowledge Base UUID")
    private String kbUuid;

    @Schema(title = "来源文档id,以逗号隔开 | Source Document IDs (Comma Separated)")
    private String sourceFileIds;

    @Schema(title = "问题 | Question")
    private String question;

    @Schema(title = "最终提供给LLM的提示词 | Final Prompt Provided to LLM")
    @TableField("prompt")
    private String prompt;

    @Schema(title = "提供给LLM的提示词所消耗的token数量 | Token Count Consumed by Prompt to LLM")
    private Integer promptTokens;

    @Schema(title = "答案 | Answer")
    private String answer;

    @Schema(title = "答案消耗的token | Answer Token Count")
    private Integer answerTokens;

    @Schema(title = "ai model id")
    private Long aiModelId;

    @Schema(title = "ai model platform")
    private String aiModelPlatform;
}

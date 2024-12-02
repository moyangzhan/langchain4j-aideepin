package com.moyz.adi.common.dto;

import com.baomidou.mybatisplus.annotation.TableField;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Data
public class KbQaDto {
    @Schema(title = "uuid")
    private String uuid;

    @Schema(title = "知识库uuid")
    private String kbUuid;

    @Schema(title = "来源文档id,以逗号隔开")
    private String sourceFileIds;

    @Schema(title = "问题")
    private String question;

    @Schema(title = "最终提供给LLM的提示词")
    @TableField("prompt")
    private String prompt;

    @Schema(title = "提供给LLM的提示词所消耗的token数量")
    private Integer promptTokens;

    @Schema(title = "答案")
    private String answer;

    @Schema(title = "答案消耗的token")
    private Integer answerTokens;

    @Schema(title = "ai model id")
    private Long aiModelId;

    @Schema(title = "ai model platform")
    private String aiModelPlatform;
}

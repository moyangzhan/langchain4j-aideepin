package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import com.moyz.adi.common.base.SearchEngineRespTypeHandler;
import com.moyz.adi.common.dto.SearchEngineResp;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.apache.ibatis.type.JdbcType;

@Data
@TableName("adi_ai_search_record")
@Schema(title = "AiSearchRecord对象", description = "AI搜索记录表")
public class AiSearchRecord extends BaseEntity {

    @TableField("uuid")
    private String uuid;

    @Schema(title = "问题")
    @TableField("question")
    private String question;

    @Schema(title = "Search engine's response content")
    @TableField(value = "search_engine_response", jdbcType = JdbcType.JAVA_OBJECT, typeHandler = SearchEngineRespTypeHandler.class)
    private SearchEngineResp searchEngineResp;

    @Schema(title = "最终提供给LLM的提示词")
    @TableField("prompt")
    private String prompt;

    @Schema(title = "提供给LLM的提示词所消耗的token数量")
    @TableField("prompt_tokens")
    private Integer promptTokens;

    @Schema(title = "答案")
    @TableField("answer")
    private String answer;

    @Schema(title = "答案消耗的token")
    @TableField("answer_tokens")
    private Integer answerTokens;

    @Schema(title = "提问用户uuid")
    @TableField("user_uuid")
    private String userUuid;

    @Schema(title = "提问用户id")
    @TableField("user_id")
    private Long userId;
}

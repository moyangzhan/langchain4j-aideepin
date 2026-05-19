package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import com.moyz.adi.common.base.AudioConfigTypeHandler;
import com.moyz.adi.common.vo.AudioConfig;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.apache.ibatis.type.JdbcType;

@EqualsAndHashCode(callSuper = true)
@Data
@TableName(value = "adi_character", autoResultMap = true)
@Schema(title = "角色实体 | Character Entity", description = "角色表 | Character Table")
public class Character extends BaseEntity {

    @Schema(title = "用户id | User ID")
    @TableField("user_id")
    private Long userId;

    @Schema(title = "角色uuid | Character UUID")
    @TableField("uuid")
    private String uuid;

    @Schema(title = "角色标题 | Character Title")
    @TableField("title")
    private String title;

    @Schema(title = "描述 | Description")
    @TableField("remark")
    private String remark;

    @Schema(title = "消耗的token数量 | Token Count Consumed")
    @TableField("tokens")
    private Integer tokens;

    @Schema(name = "是否开启理解上下文的功能 | Enable Context Understanding")
    @TableField("understand_context_enable")
    private Boolean understandContextEnable;

    @Schema(title = "设置系统信息(角色设定内容) | Set the system message to ai, ig: you are a lawyer")
    @TableField("ai_system_message")
    private String aiSystemMessage;

    @Schema(title = "请求LLM时的temperature | LLM Temperature")
    @TableField("llm_temperature")
    private Double llmTemperature;

    @Schema(title = "启用的mcp服务id列表 | Enabled MCP Service ID List")
    @TableField("mcp_ids")
    private String mcpIds;

    @Schema(title = "关联使用的知识库id列表 | Associated Knowledge Base ID List")
    @TableField("kb_ids")
    private String kbIds;

    @Schema(title = "设置响应内容类型：1：自动（跟随用户的输入类型，如果用户输入是音频，则响应内容也同样是音频，如果用户输入是文本，则响应内容显示文本），2：文本，3：音频 | Response Content Type: 1=Auto (Follows User Input Type), 2=Text, 3=Audio")
    @TableField("answer_content_type")
    private Integer answerContentType;

    @Schema(title = "设置聊天时音频类型的响应内容是否自动播放 | Auto-play Audio Response in Chat")
    @TableField("is_autoplay_answer")
    private Boolean isAutoplayAnswer;

    @Schema(title = "是否启用思考功能 | Enable Thinking")
    @TableField("is_enable_thinking")
    private Boolean isEnableThinking;

    @Schema(title = "是否启用网络搜索功能 | Enable Web Search")
    @TableField("is_enable_web_search")
    private Boolean isEnableWebSearch;

    @Schema(title = "外部系统对接密钥 | API key for external system integration")
    @TableField("api_key")
    private String apiKey;

    @TableField(value = "audio_config", jdbcType = JdbcType.JAVA_OBJECT, typeHandler = AudioConfigTypeHandler.class)
    private AudioConfig audioConfig;
}

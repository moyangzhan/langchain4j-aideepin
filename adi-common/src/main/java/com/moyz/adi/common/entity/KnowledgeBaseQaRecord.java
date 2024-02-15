package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.apache.ibatis.type.JdbcType;

@Data
@TableName("adi_knowledge_base_qa_record")
@Schema(title = "知识库问答记录实体", description = "知识库问答记录表")
public class KnowledgeBaseQaRecord extends BaseEntity {

    @Schema(title = "uuid")
    @TableField(value = "uuid", jdbcType = JdbcType.VARCHAR)
    private String uuid;

    @Schema(title = "知识库id")
    @TableField("kb_id")
    private Long kbId;

    @Schema(title = "知识库uuid")
    @TableField("kb_uuid")
    private String kbUuid;

    @Schema(title = "来源文档id,以逗号隔开")
    @TableField("source_file_ids")
    private String sourceFileIds;

    @Schema(title = "问题")
    @TableField("question")
    private String question;

    @Schema(title = "答案")
    @TableField("answer")
    private String answer;

    @Schema(title = "提问用户id")
    @TableField("user_id")
    private Long userId;
}

package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.apache.ibatis.type.JdbcType;

import java.io.Serializable;

@Data
@TableName("adi_knowledge_base_qa_record_reference")
@Schema(title = "知识库问答记录-引用实体", description = "知识库问答记录-引用列表")
public class KnowledgeBaseQaRecordReference implements Serializable {

    private static final long serialVersionUID = 1L;

    @TableId(type = IdType.AUTO)
    private Long id;

    @Schema(title = "问答记录ID")
    @TableField("qa_record_id")
    private Long qaRecordId;

    @Schema(title = "向量id")
    @TableField("embedding_id")
    private String embeddingId;

    @Schema(title = "分数")
    @TableField("score")
    private Double score;

    @Schema(title = "提问用户id")
    @TableField("user_id")
    private Long userId;
}

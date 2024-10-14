package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

@Data
@TableName("adi_knowledge_base_qa_record_ref_graph")
@Schema(title = "知识库问答记录-图谱引用", description = "知识库问答记录-图谱引用列表")
public class KnowledgeBaseQaRecordRefGraph implements Serializable {

    private static final long serialVersionUID = 1L;

    @TableId(type = IdType.AUTO)
    private Long id;

    @Schema(title = "问答记录ID")
    @TableField("qa_record_id")
    private Long qaRecordId;

    @Schema(title = "LLM解析出来的图谱")
    @TableField("graph_from_llm")
    private String graphFromLlm;

    @Schema(title = "从图数据库中查找得到的图谱")
    @TableField("graph_from_store")
    private String graphFromStore;

    @Schema(title = "提问用户id")
    @TableField("user_id")
    private Long userId;
}

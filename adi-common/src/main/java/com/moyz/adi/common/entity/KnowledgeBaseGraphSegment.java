package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Data
@TableName("adi_knowledge_base_graph_segment")
@Schema(title = "知识库-图谱-文本块 | Knowledge Base Graph Segment", description = "知识库文本块表 | Knowledge Base Text Segment Table")
public class KnowledgeBaseGraphSegment extends BaseEntity {
    private String uuid;

    @Schema(title = "所属知识库uuid | Knowledge Base UUID")
    @TableField("kb_uuid")
    private String kbUuid;

    @Schema(title = "所属知识点uuid | Knowledge Base Item UUID")
    @TableField("kb_item_uuid")
    private String kbItemUuid;

    @Schema(title = "内容 | Content")
    @TableField("remark")
    private String remark;

    @Schema(title = "创建用户id | Creator User ID")
    @TableField("user_id")
    private Long userId;
}

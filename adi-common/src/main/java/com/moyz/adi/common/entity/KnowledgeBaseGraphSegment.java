package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Data
@TableName("adi_knowledge_base_graph_segment")
@Schema(title = "知识库-图谱-文本块", description = "知识库文本块表")
public class KnowledgeBaseGraphSegment extends BaseEntity {
    private String uuid;

    @Schema(title = "所属知识库uuid")
    @TableField("kb_uuid")
    private String kbUuid;

    @Schema(title = "所属知识点uuid")
    @TableField("kb_item_uuid")
    private String kbItemUuid;

    @Schema(title = "内容")
    @TableField("remark")
    private String remark;

    @Schema(title = "创建用户id")
    @TableField("user_id")
    private Long userId;
}

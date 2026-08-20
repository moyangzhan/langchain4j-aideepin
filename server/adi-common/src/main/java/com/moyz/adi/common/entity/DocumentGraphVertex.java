package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.io.Serializable;
import java.time.LocalDateTime;

/**
 * 图谱段溯源账本-顶点贡献行：某段对某实体的一次贡献，含该段抽取的描述片段。
 * <p>
 * 元素级描述 = 全部贡献片段按行序聚合（读时派生，绝不存合并大字段）——停用删行即从聚合中消失。
 * 不继承 BaseEntity：账本行随启停硬增硬删，表上没有 update_time / is_deleted 列。
 */
@Data
@TableName("adi_document_graph_vertex")
public class DocumentGraphVertex implements Serializable {

    private static final long serialVersionUID = 1L;

    @TableId(type = IdType.AUTO)
    private Long id;

    @TableField("kb_uuid")
    private String kbUuid;

    @TableField("doc_uuid")
    private String docUuid;

    @TableField("segment_uuid")
    private String segmentUuid;

    @TableField("name")
    private String name;

    @TableField("entity_type")
    private String entityType;

    @TableField("description")
    private String description;

    @TableField("create_time")
    private LocalDateTime createTime;
}

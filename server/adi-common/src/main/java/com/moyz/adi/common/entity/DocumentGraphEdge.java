package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.io.Serializable;
import java.time.LocalDateTime;

/**
 * 图谱段溯源账本-边贡献行：某段对某关系的一次贡献，含该段抽取的关系描述片段与强度。
 * <p>
 * 端点名按字典序规范化存放（小者为 source_name）——图库侧对边的查找/合并是无向的，
 * 正反两次抽取必须收敛到同一元素键，否则独占判定会误删他段贡献。
 */
@Data
@TableName("adi_document_graph_edge")
public class DocumentGraphEdge implements Serializable {

    private static final long serialVersionUID = 1L;

    @TableId(type = IdType.AUTO)
    private Long id;

    @TableField("kb_uuid")
    private String kbUuid;

    @TableField("doc_uuid")
    private String docUuid;

    @TableField("segment_uuid")
    private String segmentUuid;

    @TableField("source_name")
    private String sourceName;

    @TableField("target_name")
    private String targetName;

    @TableField("description")
    private String description;

    @TableField("weight")
    private Double weight;

    @TableField("create_time")
    private LocalDateTime createTime;
}

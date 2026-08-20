package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.io.Serializable;
import java.time.LocalDateTime;

/**
 * 索引任务队列行：一切索引写入（切段/向量化/图谱抽取）的调度事实源。
 * <p>
 * 一行 = 一个可独立调度/合并/失败隔离的工作单元（(doc_uuid, segment_uuid, target_type, task_type)
 * 唯一）。文档级任务 segment_uuid 恒空串；同一文档的任务在领取时互斥（advisory lock），
 * 跨文档并行。不继承 BaseEntity：任务行按状态机硬流转，无 is_deleted 语义。
 */
@Data
@TableName("adi_index_task")
public class IndexTask implements Serializable {

    private static final long serialVersionUID = 1L;

    @TableId(type = IdType.AUTO)
    private Long id;

    @TableField("kb_uuid")
    private String kbUuid;

    @TableField("doc_uuid")
    private String docUuid;

    @TableField("user_id")
    private Long userId;

    @TableField("segment_uuid")
    private String segmentUuid;

    @TableField("target_type")
    private String targetType;

    @TableField("task_type")
    private String taskType;

    /**
     * 入队时目标业务表 index_version 的快照：文档任务取 adi_document.index_version，
     * 段任务取 adi_document_segment.index_version。检查点/结束置位不匹配即重入队最新版本。
     */
    @TableField("index_version")
    private Integer indexVersion;

    @TableField("status")
    private String status;

    @TableField("fail_reason")
    private String failReason;

    @TableField("create_time")
    private LocalDateTime createTime;

    @TableField("update_time")
    private LocalDateTime updateTime;
}

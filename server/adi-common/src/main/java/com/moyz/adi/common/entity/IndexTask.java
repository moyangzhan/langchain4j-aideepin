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
 * 一行 = 一个 (doc_uuid, segment_uuid, target_type, task_type, index_version) 工作单元
 * （部分唯一索引只约束未完成行，done 行留作历史；版本入键，新版本永远插新行）。
 * 文档级任务 segment_uuid 恒空串；同一文档的任务在领取时互斥（advisory lock），
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
     * 段任务取 adi_document_segment.index_version。参与合并键：新版本必然是新行；
     * 执行器各检查点发现版本前进即作废自身（接管清理，最新版本任务已在队列中）。
     */
    @TableField("index_version")
    private Integer indexVersion;

    @TableField("status")
    private String status;

    @TableField("fail_reason")
    private String failReason;

    /**
     * 协作式停止标志：新版本入队时对本键更旧的 running 行置 true（不动 status，
     * 同 doc 串行闸门保持关闭直到执行器在检查点自行中止）；claim 与失败复活时重置。
     */
    @TableField("stop_flag")
    private Boolean stopFlag;

    /**
     * 最近一次领取（开始执行）时刻；未运行过为 null。update_time 在终态时即结束时刻，
     * 两者之差即执行时长（供 done 历史行分析）。
     */
    @TableField("start_time")
    private LocalDateTime startTime;

    /**
     * 执行器存活证明：claim 时初始化、执行期间周期刷新。心跳超时的 running 行由轮询
     * 重置 pending（进程崩溃自愈）；与 update_time（普通审计列）语义不同，勿混用。
     */
    @TableField("heartbeat_time")
    private LocalDateTime heartbeatTime;

    @TableField("create_time")
    private LocalDateTime createTime;

    @TableField("update_time")
    private LocalDateTime updateTime;
}

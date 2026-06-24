package com.moyz.adi.common.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@Data
@NoArgsConstructor
@AllArgsConstructor
public class RefEmbeddingDto {
    private String embeddingId;
    private String text;

    /**
     * 记忆类型: semantic / episodic。仅在引用的是角色记忆时填充；
     * 知识库引用为 null。
     * <p>
     * Memory type: semantic / episodic. Populated for character memory references only;
     * null for knowledge base references.
     */
    private String memoryType;

    /**
     * 事件发生时间（仅 episodic 填充，用于按时间轴展示）。格式 {@code yyyy-MM-dd HH:mm:ss}。
     * <p>
     * Event timestamp (episodic only — for timeline display). Format {@code yyyy-MM-dd HH:mm:ss}.
     */
    private String createTime;

    /**
     * 事件类型（仅 episodic 填充）。
     * <p>
     * Event type (episodic only).
     */
    private String eventType;

    /**
     * 重要性 1-5（仅 episodic 填充）。
     * <p>
     * Importance 1-5 (episodic only).
     */
    private Integer importance;
}

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
     * 命中的向量化单元内容：qa=命中问题文本，parent_child=命中子块文本；text 模式
     * 命中即段文本、与 text 一致故不填。text 字段始终是"返回给 LLM 的展开内容"
     * （text 模式=段文本，qa=答案，parent_child=父段）。
     * 仅知识库引用填充；角色记忆引用为 null。
     */
    private String matchedText;

    /**
     * 引用来源文档的分段模式：text | qa | parent_child。
     * 仅知识库引用填充；角色记忆引用为 null。
     */
    private String segmentMode;

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

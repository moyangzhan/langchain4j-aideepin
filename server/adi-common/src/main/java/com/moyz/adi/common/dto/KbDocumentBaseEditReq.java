package com.moyz.adi.common.dto;

import jakarta.validation.constraints.NotBlank;
import lombok.Data;
import org.springframework.validation.annotation.Validated;

/**
 * 文档基础信息编辑（标题/摘要）：不动正文与分段参数，无失效与重索引副作用
 * Document base-info edit (title/brief): content and segment params untouched, no invalidation or reindex side effects
 */
@Data
@Validated
public class KbDocumentBaseEditReq {

    @NotBlank
    private String uuid;

    @NotBlank
    private String title;

    /**
     * 摘要；为空时由正文前 200 字截取（与文档编辑保存一致）
     * Brief; blank falls back to the first 200 chars of the content (same as document edit save)
     */
    private String brief;
}

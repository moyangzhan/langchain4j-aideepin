package com.moyz.adi.common.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

import java.util.List;

/**
 * Edit a QA pair (answer + question set in one submit).
 * One element = one question (UI granularity); the server normalizes each entry (newlines
 * collapse to spaces) and diffs by content: unchanged questions keep their vectors, removed
 * ones are deleted with vectors, new ones are enqueued for one rebuild.
 */
@Data
public class QaPairEditReq {

    @NotBlank
    private String docUuid;

    /**
     * 待编辑的答案段id（仅编辑，不新增）
     */
    @NotNull
    private Long answerSegmentId;

    /**
     * 答案内容
     */
    @NotBlank
    private String answerContent;

    /**
     * 问题集（一个元素一个问题）
     */
    @NotEmpty
    private List<String> questions;
}

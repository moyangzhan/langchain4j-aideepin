package com.moyz.adi.common.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

/**
 * 主表段内容编辑请求（text 段文本 / qa 答案 / parent_child 父段）。
 * text 模式编辑后重新向量化该段；qa/parent_child 仅更新关系行（不动向量）。
 */
@Data
public class DocumentSegmentEditReq {

    @NotNull
    private Long id;

    @NotBlank
    private String docUuid;

    @NotBlank
    private String content;
}

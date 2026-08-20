package com.moyz.adi.common.dto;

import jakarta.validation.constraints.NotBlank;
import lombok.Data;

/**
 * qa 模式问题编辑/新增请求。
 * <ul>
 *     <li>id 为空 + answerSegmentId 为空：新增一对问答（先建答案行再建问题行）</li>
 *     <li>id 为空 + answerSegmentId 非空：把新问题挂到已有答案</li>
 *     <li>id 非空：编辑该问题内容（重新向量化）</li>
 * </ul>
 */
@Data
public class DocumentSegmentQuestionEditReq {

    private Long id;

    @NotBlank
    private String docUuid;

    /**
     * 关联的答案段id；与 answerContent 二选一
     */
    private Long answerSegmentId;

    /**
     * 新答案内容（新增问答对时使用）；与 answerSegmentId 二选一
     */
    private String answerContent;

    @NotBlank
    private String content;
}

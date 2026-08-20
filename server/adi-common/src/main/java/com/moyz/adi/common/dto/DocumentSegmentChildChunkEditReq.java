package com.moyz.adi.common.dto;

import jakarta.validation.constraints.NotBlank;
import lombok.Data;

/**
 * parent_child 模式子块编辑/新增请求。
 * <ul>
 *     <li>id 非空：编辑子块内容（重新向量化）</li>
 *     <li>id 为空 + parentSegmentId 非空：在父段下追加子块</li>
 * </ul>
 */
@Data
public class DocumentSegmentChildChunkEditReq {

    private Long id;

    @NotBlank
    private String docUuid;

    /**
     * 追加子块时所属父段id
     */
    private Long parentSegmentId;

    @NotBlank
    private String content;
}

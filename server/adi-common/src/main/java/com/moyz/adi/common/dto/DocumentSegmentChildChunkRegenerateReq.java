package com.moyz.adi.common.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

/**
 * Parent-child mode rechunk request: child chunks are re-split from the parent segment's current content.
 */
@Data
public class DocumentSegmentChildChunkRegenerateReq {

    @NotNull
    private Long id;

    @NotBlank
    private String docUuid;
}

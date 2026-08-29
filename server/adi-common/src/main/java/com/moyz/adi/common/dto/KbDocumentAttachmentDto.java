package com.moyz.adi.common.dto;

import lombok.Data;

/**
 * Source file of a file-converted document: display name plus access url.
 */
@Data
public class KbDocumentAttachmentDto {

    private String name;

    private String url;
}

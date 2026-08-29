package com.moyz.adi.common.dto;

import jakarta.validation.constraints.NotEmpty;
import lombok.Data;

@Data
public class KbDocumentIndexBatchReq {

    @NotEmpty
    private String[] uuids;

    @NotEmpty
    private String[] indexTypes;
}

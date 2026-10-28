package com.moyz.adi.common.dto;

import jakarta.validation.constraints.NotEmpty;
import lombok.Data;

@Data
public class KbDocumentDelBatchReq {

    @NotEmpty
    private String[] uuids;
}

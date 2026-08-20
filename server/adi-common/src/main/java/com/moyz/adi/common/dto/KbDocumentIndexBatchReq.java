package com.moyz.adi.common.dto;

import lombok.Data;

@Data
public class KbDocumentIndexBatchReq {
    private String[] uuids;
    private String[] indexTypes;
}

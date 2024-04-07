package com.moyz.adi.common.dto;

import lombok.Data;

import java.util.List;

@Data
public class AiSearchResp {

    private Long minId;

    private List<AiSearchRecordResp> records;
}

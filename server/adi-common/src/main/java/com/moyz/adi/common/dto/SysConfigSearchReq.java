package com.moyz.adi.common.dto;

import lombok.Data;

import java.util.List;

@Data
public class SysConfigSearchReq {
    private String keyword;
    private List<String> names;
}

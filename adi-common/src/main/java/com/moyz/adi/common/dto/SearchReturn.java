package com.moyz.adi.common.dto;

import lombok.Data;

import java.util.List;

@Data
public class SearchReturn {
    private String errorMessage;
    private List<SearchReturnWebPage> items;
}

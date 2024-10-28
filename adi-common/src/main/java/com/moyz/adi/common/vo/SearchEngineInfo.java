package com.moyz.adi.common.vo;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.moyz.adi.common.interfaces.AbstractSearchEngineService;
import lombok.Data;

@Data
public class SearchEngineInfo {
    private String name;
    private Boolean enable;
    @JsonIgnore
    private AbstractSearchEngineService searchEngineService;
}

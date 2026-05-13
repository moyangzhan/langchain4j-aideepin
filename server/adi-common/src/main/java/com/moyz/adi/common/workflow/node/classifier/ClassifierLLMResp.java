package com.moyz.adi.common.workflow.node.classifier;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.util.List;

@Data
public class ClassifierLLMResp {
    private List<String> keywords;
    @JsonProperty("category_uuid")
    private String categoryUuid;
    @JsonProperty("category_name")
    private String categoryName;
}

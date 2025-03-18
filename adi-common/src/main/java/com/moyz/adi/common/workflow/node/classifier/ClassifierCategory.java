package com.moyz.adi.common.workflow.node.classifier;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.io.Serializable;

@Data
public class ClassifierCategory implements Serializable {

    @JsonProperty("category_uuid")
    private String categoryUuid;
    @JsonProperty("category_name")
    private String categoryName;

    /**
     * target node uuid
     */
    @JsonProperty("target_node_uuid")
    private String targetNodeUuid;
}

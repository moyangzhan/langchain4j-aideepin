package com.moyz.adi.common.workflow.node.classifier;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.util.ArrayList;
import java.util.List;

@Data
public class ClassifierNodeConfig {
    private List<ClassifierCategory> categories = new ArrayList<>();
    @JsonProperty("model_name")
    private String modelName;
}

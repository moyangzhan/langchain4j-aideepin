package com.moyz.adi.common.workflow.node.knowledgeretrieval;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;

@EqualsAndHashCode
@Data
public class KnowledgeRetrievalNodeConfig {
    @JsonProperty("knowledge_base_uuid")
    private String knowledgeBaseUuid;
    @JsonProperty("knowledge_base_name")
    private String knowledgeBaseName;
    private Double score;
    @JsonProperty("top_n")
    private Integer topN;
    @JsonProperty("is_strict")
    private Boolean isStrict;
    @JsonProperty("default_response")
    private String defaultResponse;
}

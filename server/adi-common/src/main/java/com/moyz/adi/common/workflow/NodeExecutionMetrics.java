package com.moyz.adi.common.workflow;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.moyz.adi.common.workflow.metrics.*;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.io.Serializable;

/**
 * 节点执行可观测指标（基类） | Node execution observability metrics (base class)
 * <p>
 * 各节点类型有对应的子类，通过 Jackson @JsonTypeInfo 实现多态序列化。
 * {@code defaultImpl = NodeExecutionMetrics.class} 确保旧数据（无 type 字段）能正常反序列化。
 * </p>
 */
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type", include = JsonTypeInfo.As.PROPERTY,
        defaultImpl = NodeExecutionMetrics.class)
@JsonSubTypes({
        @JsonSubTypes.Type(value = LLMMetrics.class, name = "llm"),
        @JsonSubTypes.Type(value = ImageMetrics.class, name = "image"),
        @JsonSubTypes.Type(value = HttpRequestMetrics.class, name = "http_request"),
        @JsonSubTypes.Type(value = SearchMetrics.class, name = "search"),
        @JsonSubTypes.Type(value = KnowledgeRetrievalMetrics.class, name = "knowledge_retrieval"),
        @JsonSubTypes.Type(value = MailMetrics.class, name = "mail"),
        @JsonSubTypes.Type(value = DocumentMetrics.class, name = "document"),
        @JsonSubTypes.Type(value = AgentMetrics.class, name = "agent")
})
@Data
@NoArgsConstructor
@AllArgsConstructor
public class NodeExecutionMetrics implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    /**
     * 节点执行耗时（毫秒） | Node execution duration in milliseconds
     */
    private long durationMs;
}

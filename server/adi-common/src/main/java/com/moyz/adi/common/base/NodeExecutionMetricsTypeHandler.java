package com.moyz.adi.common.base;

import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.workflow.NodeExecutionMetrics;
import lombok.extern.slf4j.Slf4j;
import org.apache.ibatis.type.BaseTypeHandler;
import org.apache.ibatis.type.JdbcType;
import org.apache.ibatis.type.MappedJdbcTypes;
import org.apache.ibatis.type.MappedTypes;
import org.postgresql.util.PGobject;

import java.sql.CallableStatement;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * Maps the {@code adi_workflow_runtime_node.metadata} jsonb column to {@link NodeExecutionMetrics}
 * and its subclasses. Polymorphic deserialization is driven by the {@code type} discriminator
 * declared on {@code NodeExecutionMetrics} via Jackson {@code @JsonTypeInfo} / {@code @JsonSubTypes},
 * so a row with {@code "type":"llm"} is deserialized as {@code LLMMetrics}, {@code "type":"agent"}
 * as {@code AgentMetrics}, etc. Rows without a type field fall back to the base class.
 */
@Slf4j
@MappedJdbcTypes({JdbcType.JAVA_OBJECT})
@MappedTypes({NodeExecutionMetrics.class})
public class NodeExecutionMetricsTypeHandler extends BaseTypeHandler<NodeExecutionMetrics> {

    @Override
    public void setNonNullParameter(PreparedStatement ps, int i, NodeExecutionMetrics parameter, JdbcType jdbcType) throws SQLException {
        PGobject jsonObject = new PGobject();
        jsonObject.setType("jsonb");
        try {
            jsonObject.setValue(JsonUtil.toJson(parameter));
            ps.setObject(i, jsonObject);
        } catch (Exception e) {
            throw new SQLException("Failed to serialize NodeExecutionMetrics to jsonb", e);
        }
    }

    @Override
    public NodeExecutionMetrics getNullableResult(ResultSet rs, String columnName) throws SQLException {
        return parse(rs.getString(columnName));
    }

    @Override
    public NodeExecutionMetrics getNullableResult(ResultSet rs, int columnIndex) throws SQLException {
        return parse(rs.getString(columnIndex));
    }

    @Override
    public NodeExecutionMetrics getNullableResult(CallableStatement cs, int columnIndex) throws SQLException {
        return parse(cs.getString(columnIndex));
    }

    private NodeExecutionMetrics parse(String json) {
        if (json == null || json.isEmpty() || "{}".equals(json)) {
            return null;
        }
        try {
            return JsonUtil.fromJson(json, NodeExecutionMetrics.class);
        } catch (Exception e) {
            log.warn("Failed to deserialize NodeExecutionMetrics, json={}", json, e);
            return null;
        }
    }
}

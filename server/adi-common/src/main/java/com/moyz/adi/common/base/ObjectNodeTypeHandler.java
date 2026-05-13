package com.moyz.adi.common.base;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.apache.commons.lang3.StringUtils;
import org.apache.ibatis.type.BaseTypeHandler;
import org.apache.ibatis.type.JdbcType;
import org.apache.ibatis.type.MappedJdbcTypes;
import org.apache.ibatis.type.MappedTypes;
import org.postgresql.util.PGobject;

import java.sql.CallableStatement;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

@MappedJdbcTypes({JdbcType.JAVA_OBJECT})
@MappedTypes({ObjectNode.class})
public class ObjectNodeTypeHandler extends BaseTypeHandler<ObjectNode> {

    private static final ObjectMapper objectMapper = new ObjectMapper();

    @Override
    public void setNonNullParameter(PreparedStatement ps, int i, ObjectNode parameter, JdbcType jdbcType)
            throws SQLException {
        PGobject jsonObject = new PGobject();
        jsonObject.setType("jsonb");
        try {
            jsonObject.setValue(parameter.toString());
            ps.setObject(i, jsonObject);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public ObjectNode getNullableResult(ResultSet rs, String columnName) throws SQLException {
        String jsonSource = rs.getString(columnName);
        return getObjectNode(jsonSource);
    }

    @Override
    public ObjectNode getNullableResult(ResultSet rs, int columnIndex) throws SQLException {
        String jsonSource = rs.getString(columnIndex);
        return getObjectNode(jsonSource);
    }

    @Override
    public ObjectNode getNullableResult(CallableStatement cs, int columnIndex) throws SQLException {
        String jsonSource = cs.getString(columnIndex);
        return getObjectNode(jsonSource);
    }

    private ObjectNode getObjectNode(String jsonSource) {
        if (StringUtils.isBlank(jsonSource)) {
            return null;
        }
        JsonNode result;
        try {
            result = objectMapper.readTree(jsonSource);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        if (result instanceof ObjectNode objectNode) {
            return objectNode;
        } else {
            throw new RuntimeException("Not a ObjectNode");
        }

    }
}
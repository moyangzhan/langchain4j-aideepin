package com.moyz.adi.common.config;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.moyz.adi.common.dto.mcp.UserMcpCustomizedParam;
import com.moyz.adi.common.util.JsonUtil;
import org.apache.ibatis.type.BaseTypeHandler;
import org.apache.ibatis.type.JdbcType;
import org.apache.ibatis.type.MappedJdbcTypes;
import org.apache.ibatis.type.MappedTypes;
import org.postgresql.util.PGobject;

import java.sql.CallableStatement;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

@MappedJdbcTypes({JdbcType.ARRAY})
@MappedTypes({List.class})
public class UserMcpSettingTypeHandler extends BaseTypeHandler<List<UserMcpCustomizedParam>> {

    private static final ObjectMapper objectMapper = new ObjectMapper();

    @Override
    public void setNonNullParameter(PreparedStatement ps, int i, List<UserMcpCustomizedParam> parameter, JdbcType jdbcType) throws SQLException {
        PGobject jsonObject = new PGobject();
        jsonObject.setType("jsonb");
        try {
            String json = objectMapper.writeValueAsString(parameter);
            jsonObject.setValue(json);
            ps.setObject(i, jsonObject);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public List<UserMcpCustomizedParam> getNullableResult(ResultSet rs, String columnName) throws SQLException {
        return jsonStringToList(rs.getString(columnName));
    }

    @Override
    public List<UserMcpCustomizedParam> getNullableResult(ResultSet rs, int columnIndex) throws SQLException {
        return jsonStringToList(rs.getString(columnIndex));
    }

    @Override
    public List<UserMcpCustomizedParam> getNullableResult(CallableStatement cs, int columnIndex) throws SQLException {
        return jsonStringToList(cs.getString(columnIndex));
    }

    private List<UserMcpCustomizedParam> jsonStringToList(String json) throws SQLException {
        if (json == null || json.isEmpty()) {
            return new ArrayList<>();
        }
        try {
            return parseJsonArray(json);
        } catch (JsonProcessingException e) {
            throw new SQLException("Error converting JSON to list", e);
        }
    }

    private List<UserMcpCustomizedParam> parseJsonArray(String jsonArray) throws JsonProcessingException {
        List<UserMcpCustomizedParam> list = JsonUtil.toList(jsonArray, UserMcpCustomizedParam.class);
        if (null == list || list.isEmpty()) {
            return new ArrayList<>();
        }
        return list;
    }
}

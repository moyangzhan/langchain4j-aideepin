package com.moyz.adi.common.base;

import com.moyz.adi.common.dto.SearchEngineResp;
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

@MappedJdbcTypes({JdbcType.JAVA_OBJECT})
@MappedTypes({SearchEngineResp.class})
public class SearchEngineRespTypeHandler extends BaseTypeHandler<SearchEngineResp> {

    @Override
    public void setNonNullParameter(PreparedStatement ps, int i, SearchEngineResp parameter, JdbcType jdbcType) {
        PGobject jsonObject = new PGobject();
        jsonObject.setType("jsonb");
        try {
            jsonObject.setValue(JsonUtil.toJson(parameter));
            ps.setObject(i, jsonObject);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public SearchEngineResp getNullableResult(ResultSet rs, String columnName) throws SQLException {
        String jsonSource = rs.getString(columnName);
        if (jsonSource != null) {
            try {
                return JsonUtil.fromJson(jsonSource, SearchEngineResp.class);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
        return null;
    }

    @Override
    public SearchEngineResp getNullableResult(ResultSet rs, int columnIndex) throws SQLException {
        String jsonSource = rs.getString(columnIndex);
        if (jsonSource != null) {
            try {
                return JsonUtil.fromJson(jsonSource, SearchEngineResp.class);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
        return null;
    }

    @Override
    public SearchEngineResp getNullableResult(CallableStatement cs, int columnIndex) throws SQLException {
        String jsonSource = cs.getString(columnIndex);
        if (jsonSource != null) {
            try {
                return JsonUtil.fromJson(jsonSource, SearchEngineResp.class);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
        return null;
    }
}
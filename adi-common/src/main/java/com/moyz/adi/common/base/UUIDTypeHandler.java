package com.moyz.adi.common.base;

import org.apache.ibatis.type.JdbcType;
import org.apache.ibatis.type.MappedJdbcTypes;
import org.apache.ibatis.type.MappedTypes;
import org.apache.ibatis.type.TypeHandler;

import java.sql.CallableStatement;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.UUID;

@MappedJdbcTypes({JdbcType.JAVA_OBJECT})
@MappedTypes({UUID.class})
public class UUIDTypeHandler implements TypeHandler<UUID> {

    @Override
    public void setParameter(PreparedStatement ps, int i, UUID parameter, JdbcType jdbcType) throws SQLException {
        if (parameter != null) {
            ps.setObject(i, parameter);
        } else {
            ps.setObject(i, null);
        }
    }

    @Override
    public UUID getResult(ResultSet rs, String columnName) throws SQLException {
        String uuidString = rs.getString(columnName);
        return uuidString == null ? null : UUID.fromString(uuidString);
    }

    @Override
    public UUID getResult(ResultSet rs, int columnIndex) throws SQLException {
        String uuidString = rs.getString(columnIndex);
        return uuidString == null ? null : UUID.fromString(uuidString);
    }

    @Override
    public UUID getResult(CallableStatement cs, int columnIndex) throws SQLException {
        String uuidString = cs.getString(columnIndex);
        return uuidString == null ? null : UUID.fromString(uuidString);
    }
}
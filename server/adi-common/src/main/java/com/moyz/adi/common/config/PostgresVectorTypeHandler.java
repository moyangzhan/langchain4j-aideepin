package com.moyz.adi.common.config;

import com.pgvector.PGvector;
import org.apache.ibatis.type.BaseTypeHandler;
import org.apache.ibatis.type.JdbcType;

import java.sql.CallableStatement;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

public class PostgresVectorTypeHandler extends BaseTypeHandler<PGvector> {

    @Override
    public void setNonNullParameter(PreparedStatement ps, int i, PGvector parameter, JdbcType jdbcType)
            throws SQLException {
        ps.setObject(i, parameter);
//        ps.setArray(i, ps.getConnection().createArrayOf("float", parameter));
    }

    @Override
    public PGvector getNullableResult(ResultSet rs, String columnName) throws SQLException {
        return toFloatArray(rs.getArray(columnName));
    }

    @Override
    public PGvector getNullableResult(ResultSet rs, int columnIndex) throws SQLException {
        return toFloatArray(rs.getArray(columnIndex));
    }

    @Override
    public PGvector getNullableResult(CallableStatement cs, int columnIndex) throws SQLException {
        return toFloatArray(cs.getArray(columnIndex));
    }

    private PGvector toFloatArray(java.sql.Array sqlArray) throws SQLException {
        PGvector pGvector = new PGvector(new float[0]);
        if (sqlArray == null) {
            return pGvector;
        }
        pGvector.setValue(sqlArray.toString());
        return pGvector;
    }

}
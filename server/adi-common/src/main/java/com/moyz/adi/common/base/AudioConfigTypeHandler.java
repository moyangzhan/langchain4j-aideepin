package com.moyz.adi.common.base;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.vo.AudioConfig;
import com.moyz.adi.common.workflow.WfNodeInputConfig;
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

@Slf4j
@MappedJdbcTypes({JdbcType.JAVA_OBJECT})
@MappedTypes({WfNodeInputConfig.class})
public class AudioConfigTypeHandler extends BaseTypeHandler<AudioConfig> {

    @Override
    public void setNonNullParameter(PreparedStatement ps, int i, AudioConfig parameter, JdbcType jdbcType) {
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
    public AudioConfig getNullableResult(ResultSet rs, String columnName) throws SQLException {
        String jsonSource = rs.getString(columnName);
        if (jsonSource != null) {
            try {
                return fillValue(jsonSource);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
        return null;
    }

    @Override
    public AudioConfig getNullableResult(ResultSet rs, int columnIndex) throws SQLException {
        String jsonSource = rs.getString(columnIndex);
        if (jsonSource != null) {
            return fillValue(jsonSource);
        }
        return null;
    }

    @Override
    public AudioConfig getNullableResult(CallableStatement cs, int columnIndex) throws SQLException {
        String jsonSource = cs.getString(columnIndex);
        if (jsonSource != null) {
            try {
                return fillValue(jsonSource);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
        return null;
    }

    public static AudioConfig fillValue(String jsonSource) {
        ObjectNode jsonNode = (ObjectNode) JsonUtil.toJsonNode(jsonSource);

        if (null == jsonNode) {
            AudioConfig result = new AudioConfig();
            result.setVoice(new AudioConfig.Voice());
            return new AudioConfig();
        }
        return JsonUtil.fromJson(jsonNode, AudioConfig.class);
    }

}
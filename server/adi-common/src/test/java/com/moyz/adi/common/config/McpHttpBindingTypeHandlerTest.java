package com.moyz.adi.common.config;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.moyz.adi.common.dto.mcp.McpCommonParam;
import com.moyz.adi.common.dto.mcp.McpCustomizedParamDefinition;
import org.apache.ibatis.type.JdbcType;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.postgresql.util.PGobject;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

/** Executes the unchanged JSONB handlers with synthetic JDBC endpoints, without a database server. */
class McpHttpBindingTypeHandlerTest {
    private final ObjectMapper json = new ObjectMapper();

    @Test
    void presetBindingSurvivesTheActualJsonbWriteReadPathAndLegacyNullsRemainNull() throws Exception {
        McpCommonParam param = json.readValue("{\"name\":\"fixed\",\"value\":\"fixture-fixed\",\"bind_type\":\"header\",\"bind_name\":\"X-Fixed\"}", McpCommonParam.class);
        McpPresetParamTypeHandler handler = new McpPresetParamTypeHandler();
        PreparedStatement statement = mock(PreparedStatement.class);
        handler.setNonNullParameter(statement, 1, List.of(param), JdbcType.OTHER);
        ArgumentCaptor<PGobject> saved = ArgumentCaptor.forClass(PGobject.class);
        verify(statement).setObject(eq(1), saved.capture());
        assertEquals("jsonb", saved.getValue().getType());
        ResultSet result = mock(ResultSet.class);
        when(result.getString("preset_params")).thenReturn(saved.getValue().getValue());
        McpCommonParam restored = handler.getNullableResult(result, "preset_params").get(0);
        assertEquals("header", restored.getBindType());
        assertEquals("X-Fixed", restored.getBindName());
        when(result.getString("preset_params")).thenReturn("[{\"name\":\"legacy\",\"value\":\"value\"}]");
        McpCommonParam legacy = handler.getNullableResult(result, "preset_params").get(0);
        assertNull(legacy.getBindType());
        assertNull(legacy.getBindName());
    }

    @Test
    void customizedTemplateSurvivesTheActualJsonbWriteReadPath() throws Exception {
        McpCustomizedParamDefinition definition = json.readValue("{\"name\":\"token\",\"require_encrypt\":true,\"bind_type\":\"header\",\"bind_name\":\"Authorization\",\"bind_value_template\":\"Bearer {value}\"}", McpCustomizedParamDefinition.class);
        McpCustomizedParamDefinitionTypeHandler handler = new McpCustomizedParamDefinitionTypeHandler();
        PreparedStatement statement = mock(PreparedStatement.class);
        handler.setNonNullParameter(statement, 1, List.of(definition), JdbcType.OTHER);
        ArgumentCaptor<PGobject> saved = ArgumentCaptor.forClass(PGobject.class);
        verify(statement).setObject(eq(1), saved.capture());
        ResultSet result = mock(ResultSet.class);
        when(result.getString(1)).thenReturn(saved.getValue().getValue());
        McpCustomizedParamDefinition restored = handler.getNullableResult(result, 1).get(0);
        assertEquals("token", restored.getName());
        assertEquals("header", restored.getBindType());
        assertEquals("Authorization", restored.getBindName());
        assertEquals("Bearer {value}", restored.getBindValueTemplate());
        assertTrue(restored.getRequireEncrypt());
    }
}

package com.moyz.adi.common.util;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.moyz.adi.common.dto.mcp.McpCommonParam;
import com.moyz.adi.common.dto.mcp.McpCustomizedParamDefinition;
import com.moyz.adi.common.dto.mcp.UserMcpCustomizedParam;
import com.moyz.adi.common.entity.Mcp;
import com.moyz.adi.common.entity.UserMcp;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

class McpHttpParametersTest {
    private final ObjectMapper mapper = new ObjectMapper();

    @Test
    void legacyQueryUsesStableNamesAndUserOverride() throws Exception {
        Mcp mcp = mcp("[{\"name\":\"token\",\"value\":\"preset\"}]", "[{\"name\":\"token\"}]");
        McpHttpParameters result = McpHttpParameters.resolve(mcp, user("[{\"name\":\"token\",\"value\":\"user a&b=中文\"}]"));
        assertEquals("https://mcp.example.com/mcp?profile=public&token=user%20a%26b%3D%E4%B8%AD%E6%96%87#anchor", result.url());
        assertEquals(Map.of(), result.headers());
    }

    @Test
    void headerValuesNeverBecomeQueryAndMatchingIsPerDefinition() throws Exception {
        Mcp mcp = mcp("[{\"name\":\"fixed\",\"value\":\"fixed-value\",\"bind_type\":\"header\",\"bind_name\":\"X-Fixed\"}]",
                "[{\"name\":\"token\",\"bind_type\":\"header\",\"bind_name\":\"Authorization\",\"bind_value_template\":\"Bearer {value}\"},"
                        + "{\"name\":\"other\",\"bind_type\":\"header\",\"bind_name\":\"X-Other\",\"bind_value_template\":\"{value}\"},"
                        + "{\"name\":\"legacy\"},{\"name\":\"alias\",\"bind_name\":\"renamed\"}]");
        McpHttpParameters result = McpHttpParameters.resolve(mcp, user("[{\"name\":\"token\",\"value\":\"first\"},{\"name\":\"other\",\"value\":\"second\"},{\"name\":\"legacy\",\"value\":\"query\"},{\"name\":\"alias\",\"value\":\"q2\"}]"));
        assertEquals(Map.of("Authorization", "Bearer first", "X-Other", "second", "X-Fixed", "fixed-value"), result.headers());
        assertTrue(result.url().contains("legacy=query&renamed=q2"));
        assertFalse(result.url().contains("first"));
        assertFalse(result.url().contains("second"));
        assertFalse(result.url().contains("fixed-value"));
    }

    @Test
    void noOrEmptyBindingsPreserveUrlAndIgnoreUndeclaredUserValues() throws Exception {
        for (String definitions : List.of("null", "[]")) {
            Mcp mcp = mcp("null", definitions);
            McpHttpParameters result = McpHttpParameters.resolve(mcp, user("[{\"name\":\"unbound\",\"value\":\"must-not-fallback\"}]"));
            assertEquals(mcp.getSseUrl(), result.url());
            assertTrue(result.headers().isEmpty());
        }
    }

    @Test
    void missingLegacyValuesRemainOptionalButNullNeverBecomesLiteralNull() throws Exception {
        Mcp mcp = mcp("[{\"name\":\"fixed\",\"value\":null}]", "[{\"name\":\"empty\"},{\"name\":\"absent\"}]");
        assertEquals(mcp.getSseUrl(), McpHttpParameters.resolve(mcp, user("[{\"name\":\"empty\",\"value\":\"\"}]")).url());
    }

    @Test
    void missingBoundHeaderFailsWithoutFallingBackToOtherUserValues() throws Exception {
        Mcp mcp = mcp("[]", "[{\"name\":\"expected\",\"bind_type\":\"header\",\"bind_name\":\"Authorization\"}]");
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> McpHttpParameters.resolve(mcp, user("[{\"name\":\"different\",\"value\":\"synthetic-secret\"}]")));
        assertFalse(ex.toString().contains("synthetic-secret"));
    }

    @ParameterizedTest
    @NullAndEmptySource
    @ValueSource(strings = {" ", "\t"})
    void missingOrBlankExplicitValueFails(String value) throws Exception {
        Mcp mcp = mcp("[]", "[{\"name\":\"token\",\"bind_type\":\"header\",\"bind_name\":\"Authorization\",\"bind_value_template\":\"Bearer {value}\"}]");
        UserMcpCustomizedParam param = new UserMcpCustomizedParam();
        param.setName("token");
        param.setValue(value);
        UserMcp user = new UserMcp();
        user.setMcpCustomizedParams(List.of(param));
        assertThrows(IllegalArgumentException.class, () -> McpHttpParameters.resolve(mcp, user));
    }

    @ParameterizedTest
    @ValueSource(strings = {"", "unknown", "HEADER", " "})
    void invalidBindingTypesFailInsteadOfQueryFallback(String type) throws Exception {
        Mcp mcp = mcp("[]", "[{\"name\":\"token\"}]");
        mcp.getCustomizedParamDefinitions().get(0).setBindType(type);
        assertThrows(IllegalArgumentException.class, () -> McpHttpParameters.resolve(mcp, user("[{\"name\":\"token\",\"value\":\"synthetic-secret\"}]")));
    }

    @ParameterizedTest
    @ValueSource(strings = {"", " ", "Bearer", "Bearer {other}", "Bearer ${value}", "Bearer {value}\r\nX"})
    void emptyOrUnknownTemplatesFail(String template) throws Exception {
        Mcp mcp = mcp("[]", "[{\"name\":\"token\",\"bind_type\":\"header\"}]");
        mcp.getCustomizedParamDefinitions().get(0).setBindValueTemplate(template);
        assertThrows(IllegalArgumentException.class, () -> McpHttpParameters.resolve(mcp, user("[{\"name\":\"token\",\"value\":\"synthetic-secret\"}]")));
    }

    @Test
    void absentTemplateIsRawValueAndReplacementDoesNotRecurse() throws Exception {
        Mcp mcp = mcp("[]", "[{\"name\":\"token\",\"bind_type\":\"header\",\"bind_name\":\"X-Token\"}]");
        assertEquals("raw{value}", McpHttpParameters.resolve(mcp, user("[{\"name\":\"token\",\"value\":\"raw{value}\"}]")).headers().get("X-Token"));
        mcp.getCustomizedParamDefinitions().get(0).setBindValueTemplate("Bearer {value}");
        assertEquals("Bearer raw{value}", McpHttpParameters.resolve(mcp, user("[{\"name\":\"token\",\"value\":\"raw{value}\"}]")).headers().get("X-Token"));
    }

    @Test
    void headerNameCollisionIsCaseInsensitiveAcrossPresetAndUser() throws Exception {
        Mcp mcp = mcp("[{\"name\":\"fixed\",\"value\":\"one\",\"bind_type\":\"header\",\"bind_name\":\"Authorization\"}]",
                "[{\"name\":\"token\",\"bind_type\":\"header\",\"bind_name\":\"authorization\"}]");
        assertThrows(IllegalArgumentException.class, () -> McpHttpParameters.resolve(mcp, user("[{\"name\":\"token\",\"value\":\"two\"}]")));
    }

    @Test
    void explicitQueryCollisionAndDuplicateUserIdentityAreRejected() throws Exception {
        Mcp mcp = mcp("[{\"name\":\"fixed\",\"value\":\"one\",\"bind_name\":\"target\"}]", "[{\"name\":\"token\",\"bind_name\":\"target\"}]");
        assertThrows(IllegalArgumentException.class, () -> McpHttpParameters.resolve(mcp, user("[{\"name\":\"token\",\"value\":\"two\"}]")));
        assertThrows(IllegalArgumentException.class, () -> McpHttpParameters.resolve(mcp("[]", "[]"), user("[{\"name\":\"token\",\"value\":\"one\"},{\"name\":\"token\",\"value\":\"two\"}]")));
    }

    @ParameterizedTest
    @ValueSource(strings = {"", "Bad Name", "X-Test\r\nInjected", "X-Test:"})
    void malformedHeaderNamesAreRejected(String name) throws Exception {
        Mcp mcp = mcp("[]", "[{\"name\":\"token\",\"bind_type\":\"header\"}]");
        mcp.getCustomizedParamDefinitions().get(0).setBindName(name);
        assertThrows(IllegalArgumentException.class, () -> McpHttpParameters.resolve(mcp, user("[{\"name\":\"token\",\"value\":\"synthetic-secret\"}]")));
    }

    @Test
    void headerNewlineAndNonScalarValuesAreRejectedWithoutValueInError() throws Exception {
        Mcp mcp = mcp("[]", "[{\"name\":\"token\",\"bind_type\":\"header\"}]");
        for (Object value : List.of("synthetic-secret\r\nInjected", Map.of("token", "synthetic-secret"))) {
            UserMcpCustomizedParam param = new UserMcpCustomizedParam();
            param.setName("token");
            param.setValue(value);
            UserMcp user = new UserMcp();
            user.setMcpCustomizedParams(List.of(param));
            assertFalse(assertThrows(IllegalArgumentException.class, () -> McpHttpParameters.resolve(mcp, user)).toString().contains("synthetic-secret"));
        }
    }

    @Test
    void changingRequestNameDoesNotRequireMigratingStoredUserName() throws Exception {
        Mcp mcp = mcp("[]", "[{\"name\":\"token\",\"bind_type\":\"header\",\"bind_name\":\"X-First\"}]");
        UserMcp user = user("[{\"name\":\"token\",\"value\":\"value\"}]");
        assertEquals(Map.of("X-First", "value"), McpHttpParameters.resolve(mcp, user).headers());
        mcp.getCustomizedParamDefinitions().get(0).setBindName("X-Second");
        assertEquals(Map.of("X-Second", "value"), McpHttpParameters.resolve(mcp, user).headers());
    }

    private Mcp mcp(String presets, String definitions) throws Exception {
        Mcp mcp = new Mcp();
        mcp.setSseUrl("https://mcp.example.com/mcp?profile=public#anchor");
        mcp.setPresetParams(presets.equals("null") ? null : mapper.readValue(presets, mapper.getTypeFactory().constructCollectionType(List.class, McpCommonParam.class)));
        mcp.setCustomizedParamDefinitions(definitions.equals("null") ? null : mapper.readValue(definitions, mapper.getTypeFactory().constructCollectionType(List.class, McpCustomizedParamDefinition.class)));
        return mcp;
    }

    private UserMcp user(String params) throws Exception {
        UserMcp user = new UserMcp();
        user.setMcpCustomizedParams(mapper.readValue(params, mapper.getTypeFactory().constructCollectionType(List.class, UserMcpCustomizedParam.class)));
        return user;
    }
}

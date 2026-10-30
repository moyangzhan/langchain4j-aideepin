package com.moyz.adi.common.util;

import com.fasterxml.jackson.annotation.JsonInclude;
import tools.jackson.core.JacksonException;
import tools.jackson.core.JsonGenerator;
import tools.jackson.core.JsonParser;
import tools.jackson.core.type.TypeReference;
import tools.jackson.databind.DeserializationFeature;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.SerializationFeature;
import tools.jackson.databind.json.JsonMapper;
import tools.jackson.databind.node.ArrayNode;
import tools.jackson.databind.node.ObjectNode;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import java.io.StringWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Slf4j
public class JsonUtil {

    private static final ObjectMapper objectMapper = JsonMapper.builder()
            .changeDefaultPropertyInclusion(value -> value.withValueInclusion(JsonInclude.Include.NON_NULL))
            .disable(SerializationFeature.INDENT_OUTPUT)
            .disable(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES)
            .addModule(LocalDateTimeUtil.getSimpleModule())
            .build();

    public static final ObjectMapper getObjectMapper() {
        return objectMapper;
    }

    public static String toJson(Object obj) {
        String resp = null;
        try {
            resp = objectMapper.writeValueAsString(obj);
        } catch (JacksonException e) {
            log.error("JsonUtil error", e);
        }
        return resp;
    }


    /**
     * 创建JSON处理器的静态方法
     *
     * @param content JSON字符串
     * @return
     */
    private static JsonParser getParser(String content) {
        if (StringUtils.isNotBlank(content)) {
            try {
                return objectMapper.tokenStreamFactory().createParser(content);
            } catch (JacksonException e) {
                log.error("JsonUtil getParser error", e);
            }
        }
        return null;
    }

    /**
     * 创建JSON生成器的静态方法, 使用标准输出
     *
     * @return
     */
    private static JsonGenerator getGenerator(StringWriter sw) {
        try {
            return objectMapper.tokenStreamFactory().createGenerator(sw);
        } catch (JacksonException e) {
            log.error("JsonUtil getGenerator error", e);
        }
        return null;
    }

    /**
     * JSON对象反序列化
     */
    public static <T> T fromJson(String json, Class<T> clazz) {
        if (StringUtils.isBlank(json)) {
            return null;
        }
        try {
            JsonParser jp = getParser(json);
            if (null == jp) {
                log.error("json parser is null");
                return null;
            }
            return jp.readValueAs(clazz);
        } catch (JacksonException e) {
            log.error("Deserialization failed", e);
        }
        return null;
    }

    public static <T> T fromJson(JsonNode jsonNode, Class<T> clazz) {
        try {
            return objectMapper.treeToValue(jsonNode, clazz);
        } catch (JacksonException e) {
            log.error("Deserialization failed", e);
        }
        return null;
    }


    public static <T> List<T> fromArrayNode(ArrayNode arrayNode, Class<T> clazz) {
        List<T> result = new ArrayList<>();
        try {
            for (JsonNode jsonNode : arrayNode) {
                result.add(objectMapper.treeToValue(jsonNode, clazz));
            }
        } catch (JacksonException e) {
            log.error("Deserialization failed", e);
        }
        return result;
    }

    public static JsonNode toJsonNode(String json) {
        if (StringUtils.isBlank(json)) {
            return null;
        }
        try {
            return objectMapper.readTree(json);
        } catch (JacksonException e) {
            log.error("Deserialization failed", e);
        }
        return null;
    }

    public static Map<String, Object> toMap(String json) {
        Map<String, Object> result;
        try {
            result = objectMapper.readValue(json, Map.class);
        } catch (JacksonException e) {
            throw new RuntimeException(e);
        }
        return result;
    }

    public static <T> List<T> toList(String json, Class<T> clazz) {
        try {
            return objectMapper.readValue(json, objectMapper.getTypeFactory().constructCollectionType(List.class, clazz));
        } catch (JacksonException e) {
            log.error("Deserialization failed", e);
        }
        return null;
    }

    public static Map<String, Object> toMap(Object obj) {
        try {
            return objectMapper.convertValue(obj, new TypeReference<HashMap<String, Object>>() {
            });
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    public static JsonNode classToJsonNode(Object obj) {
        return objectMapper.valueToTree(obj);
    }

    public static ObjectNode createObjectNode() {
        return objectMapper.createObjectNode();
    }

    public static ArrayNode createArrayNode() {
        return objectMapper.createArrayNode();
    }

}
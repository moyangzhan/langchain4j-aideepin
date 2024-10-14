package com.moyz.adi.common.util;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.*;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.io.StringWriter;
import java.util.HashMap;
import java.util.Map;

@Slf4j
@Component
public class JsonUtil {

    private static final ObjectMapper objectMapper = new ObjectMapper();

    static {
        objectMapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
        objectMapper.configure(SerializationFeature.INDENT_OUTPUT, Boolean.FALSE);
        objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        objectMapper.registerModules(LocalDateTimeUtil.getSimpleModule(), new JavaTimeModule(), new Jdk8Module());
    }

    public static String toJson(Object obj) {
        String resp = null;
        try {
            resp = objectMapper.writeValueAsString(obj);
        } catch (JsonGenerationException e) {
            log.error("JsonUtil error", e);
        } catch (JsonMappingException e) {
            log.error("JsonUtil error", e);
        } catch (IOException e) {
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
                return objectMapper.getFactory().createParser(content);
            } catch (IOException ioe) {
                log.error("JsonUtil getParser error", ioe);
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
            return objectMapper.getFactory().createGenerator(sw);
        } catch (IOException e) {
            log.error("JsonUtil getGenerator error", e);
        }
        return null;
    }

    /**
     * JSON对象反序列化
     */
    public static <T> T fromJson(String json, Class<T> clazz) {
        try {
            JsonParser jp = getParser(json);
            return jp.readValueAs(clazz);
        } catch (JsonParseException jpe) {
            log.error("反序列化失败", jpe);
        } catch (JsonMappingException jme) {
            log.error("反序列化失败", jme);
        } catch (IOException ioe) {
            log.error("反序列化失败", ioe);
        }
        return null;
    }

    public static Map<String, Object> toMap(String json) {
        Map<String, Object> result;
        try {
            result = objectMapper.readValue(json, Map.class);
        } catch (JsonProcessingException e) {
            throw new RuntimeException(e);
        }
        return result;
    }

    public static Map<String, Object> toMap(Object obj) {
        Map<String, Object> result;
        try {
            result = objectMapper.convertValue(obj, Map.class);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        return result;
    }

}
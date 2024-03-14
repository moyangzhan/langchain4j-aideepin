package com.moyz.adi.common.util;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonGenerationException;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonParser;
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

    public static String toJsonString(Object obj) {
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
        if(StringUtils.isNotBlank(content)){
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
     * JSON对象序列化
     */
    public static String toJson(Object obj) {
        StringWriter sw = new StringWriter();
        JsonGenerator jsonGen = getGenerator(sw);
        if (jsonGen == null) {
            try {
                sw.close();
            } catch (IOException e) {
                log.error("JsonUtil toJSON error", e);
            }
            return null;
        }
        try {
            //由于在getGenerator方法中指定了OutputStream为sw
            //因此调用writeObject会将数据输出到sw
            jsonGen.writeObject(obj == null ? "" : obj);
            //由于采用流式输出 在输出完毕后务必清空缓冲区并关闭输出流
            jsonGen.flush();
            jsonGen.close();
            return sw.toString();
        } catch (JsonGenerationException jge) {
            log.error("JSON生成错误", jge);
        } catch (IOException ioe) {
            log.error("JSON输入输出错误", ioe);
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

}
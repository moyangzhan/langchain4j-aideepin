package com.moyz.adi.common.util;

import com.fasterxml.jackson.databind.module.SimpleModule;
import com.fasterxml.jackson.databind.ser.std.ToStringSerializer;
import org.apache.commons.lang3.StringUtils;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;

public class LocalDateTimeUtil {

    public static SimpleModule getSimpleModule() {
        // jackson中自定义处理序列化和反序列化
        SimpleModule customModule = new SimpleModule();
        customModule.addSerializer(Long.class, ToStringSerializer.instance);
        // 时间序列化
        customModule.addSerializer(LocalDateTime.class, new LocalDateTimeSerializer());
        customModule.addDeserializer(LocalDateTime.class, new LocalDateTimeDeserializer());
        return customModule;
    }

    public static LocalDateTime parse(String localDateTime) {
        return LocalDateTime.parse(localDateTime, DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
    }

    public static LocalDateTime parse(Long epochMilli) {
        return LocalDateTime.ofInstant(Instant.ofEpochMilli(epochMilli), ZoneId.systemDefault());
    }

    public static String format(LocalDateTime localDateTime) {
        if (null == localDateTime) {
            return StringUtils.EMPTY;
        }
        return localDateTime.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
    }

    public static String format(LocalDateTime localDateTime, String pattern) {
        if (null == localDateTime) {
            return StringUtils.EMPTY;
        }
        return localDateTime.format(DateTimeFormatter.ofPattern(pattern));
    }

    public static int getIntDay(LocalDateTime localDateTime) {
        return localDateTime.getYear() * 10000 + localDateTime.getMonthValue() * 100 + localDateTime.getDayOfMonth();
    }

    public static int getToday() {
        LocalDateTime now = LocalDateTime.now();
        return now.getYear() * 10000 + now.getMonthValue() * 100 + now.getDayOfMonth();
    }
}

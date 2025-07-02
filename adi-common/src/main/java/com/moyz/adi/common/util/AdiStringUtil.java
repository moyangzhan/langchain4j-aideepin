package com.moyz.adi.common.util;

import org.jsoup.Jsoup;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

public class AdiStringUtil {

    public static String clearStr(String str) {
        org.jsoup.nodes.Document doc = Jsoup.parse(str);
        return doc.text();
    }

    public static String tail(String source, int tailLength) {
        if (source.length() <= tailLength) {
            return source;
        }
        return source.substring(source.length() - tailLength);
    }

    /**
     * 支持将字符串按分隔符切割并转换为List，支持基础类型对应的字符串
     *
     * @param str       待转换的字符串
     * @param separator 分隔符
     * @param function  转换函数，将字符串转换为目标类型
     * @param <T>       列表元素类型
     * @return 转换后的List
     */
    public static <T> List<T> stringToList(String str, String separator, Function<String, T> function) {
        if (str == null || str.isEmpty()) {
            return List.of();
        }
        String[] parts = str.split(separator);
        List<T> result = new ArrayList<>();
        for (String part : parts) {
            if (part != null && !part.isEmpty()) {
                result.add(function.apply(part));
            }
        }
        return result;
    }
}

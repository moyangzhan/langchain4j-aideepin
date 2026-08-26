package com.moyz.adi.common.util;

import com.fasterxml.jackson.databind.JsonNode;
import org.jsoup.Jsoup;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class AdiStringUtil {

    public static String clearStr(String str) {
        org.jsoup.nodes.Document doc = Jsoup.parse(str);
        return doc.text();
    }

    /**
     * Normalize to a single line: newlines (and \r) with surrounding whitespace collapse to
     * one space (plain removal would glue English words together), runs of whitespace shrink
     * to one space, ends trimmed.
     * <p>
     * "One question = one input" is guaranteed by UI granularity; this is the API-level
     * backstop — copied or imported content may contain newlines the user cannot see, and
     * splitting on them would silently corrupt the question. Normalization upholds the
     * invariant "stored questions never contain newlines".
     */
    public static String normalizeSingleLine(String text) {
        if (text == null) {
            return null;
        }
        return text.replaceAll("[\\r\\n]+", " ").replaceAll("\\s{2,}", " ").trim();
    }

    /**
     * Extract the message field from text that looks like a JSON error body. LLM/embedding
     * providers often return the whole body as the exception message (e.g.
     * {"code":30003,"message":"Model disabled.","data":null}), which is unreadable when
     * stored and shown to users. Returns the input unchanged when it is not JSON or has no
     * message. A trailing period is dropped so suffixes like ", name: xxx" append cleanly.
     */
    public static String extractJsonMessage(String text) {
        if (text == null) {
            return null;
        }
        String trimmed = text.strip();
        if (trimmed.startsWith("{") && trimmed.endsWith("}")) {
            JsonNode node = JsonUtil.toJsonNode(trimmed);
            if (node != null && node.hasNonNull("message")) {
                String message = node.get("message").asText().strip();
                if (!message.isEmpty()) {
                    return message.endsWith(".") ? message.substring(0, message.length() - 1) : message;
                }
            }
        }
        return text;
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

    public static String removeSpecialChar(String input) {
// Match common special characters (including Chinese and English symbols)
        // 匹配常见特殊符号（包括中英文符号）
        String regEx = "[\\-`~!@#$%^&*()+=|{}':;,.<>/?！￥…（）—【】‘；：”“’。，、？]";
        Pattern p = Pattern.compile(regEx);
        Matcher m = p.matcher(input);
        return m.replaceAll("").trim(); // 替换为空字符串并去除首尾空格
    }

    public static String removeCodeBlock(String input) {
        String regEx = "^```[a-zA-Z0-9]*\\n([\\s\\S]*?)\\n```$";
        Pattern p = Pattern.compile(regEx);
        Matcher m = p.matcher(input);
        return m.replaceAll("").trim(); // 替换为空字符串并去除首尾空格
    }
}

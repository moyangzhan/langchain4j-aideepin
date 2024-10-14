package com.moyz.adi.common.util;

import com.moyz.adi.common.vo.GraphSearchCondition;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;

import java.util.*;
import java.util.stream.Collectors;

public class GraphStoreUtil {

    public static String buildMatchClause(String name, String textSegmentId) {
        List<String> criteriaList = new ArrayList<>();
        if (StringUtils.isNotBlank(name)) {
            criteriaList.add("name:$name");
        }
        if (StringUtils.isNotBlank(textSegmentId)) {
            criteriaList.add("textSegmentId:$textSegmentId");
        }
        String matchSql = "{%s}";
        return String.format(matchSql, criteriaList.stream().collect(Collectors.joining(",")));
    }

    public static Map<String, Object> buildMatchArgs(String name, String textSegmentId) {
        Map<String, Object> result = new HashMap<>();
        if (StringUtils.isNotBlank(name)) {
            result.put("name", name);
        }
        if (StringUtils.isNotBlank(textSegmentId)) {
            result.put("textSegmentId", textSegmentId);
        }
        return result;
    }

    public static String buildWhereClause(GraphSearchCondition search, String alias, String varPrefix) {
        if (null == search) {
            return StringUtils.EMPTY;
        }
        StringBuilder whereClause = new StringBuilder();
        if (CollectionUtils.isNotEmpty(search.getNames())) {
            whereClause.append(String.format("(%s.name in [$%s_name])", alias, varPrefix));
        }
        //Metadata直接拼接字符串，要防SQL注入
        if (null != search.getMetadataFilter()) {
            if (whereClause.length() > 0) {
                whereClause.append(" and ");
            }
            AdiApacheAgeJSONFilterMapper adiJSONFilterMapper = new AdiApacheAgeJSONFilterMapper("metadata");
            adiJSONFilterMapper.setAlias(alias);
            String metadataWhereClause = adiJSONFilterMapper.map(search.getMetadataFilter());
            whereClause.append(metadataWhereClause);
        }

        return whereClause.toString();
    }

    public static Map<String, Object> buildWhereArgs(GraphSearchCondition search, String varPrefix) {
        if (null == search) {
            return Collections.emptyMap();
        }
        Map<String, Object> result = new HashMap<>();
        if (CollectionUtils.isNotEmpty(search.getNames())) {
            result.put(varPrefix + "_name", search.getNames().stream().collect(Collectors.joining(",")));
        }
        return result;
    }

    public static String buildSetClause(Map<String, Object> metadata) {
        //Apache AGE不支持直接更新property中的Map或List，只能直接替换，否则会出现异常：ERROR:  SET clause doesn't not support updating maps or lists in a property
        StringBuilder setClause = new StringBuilder();
        if (MapUtils.isNotEmpty(metadata)) {
            setClause.append(String.format(",v.metadata=$new_metadata"));
        }
        return setClause.toString();
    }

    public static Map<String, Object> buildSetArgs(Map<String, Object> metadata) {
        Map<String, Object> result = new HashMap<>();
        if (MapUtils.isNotEmpty(metadata)) {
            result.put("new_metadata", metadata);
        }

        return result;
    }
}

package com.moyz.adi.common.util;

import org.apache.commons.lang3.SerializationUtils;

import java.io.Serializable;
import java.util.List;
import java.util.stream.Collectors;

public class CollectionUtil {
    public static <T extends Serializable> List<T> deepCopy(List<T> source) {
        return source.stream().map(SerializationUtils::clone).collect(Collectors.toList());
    }
}

package com.moyz.adi.common.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.Arrays;

@Getter
@AllArgsConstructor
public enum EmbeddingStatusEnum implements BaseEnum {
    NONE(1, "Not Embedded"),
    DOING(2, "Embedding"),
    DONE(3, "Embedded"),
    FAIL(4, "Embedding Failed");

    private final Integer value;
    private final String desc;

    public static EmbeddingStatusEnum getByValue(Integer val) {
        return Arrays.stream(EmbeddingStatusEnum.values()).filter(item -> item.value.equals(val)).findFirst().orElse(null);
    }
}

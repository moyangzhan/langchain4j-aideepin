package com.moyz.adi.common.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.Arrays;

@Getter
@AllArgsConstructor
public enum GraphicalStatusEnum implements BaseEnum {
    NONE(1, "Not Built"),
    DOING(2, "Building"),
    DONE(3, "Built"),
    FAIL(4, "Build Failed");

    private final Integer value;
    private final String desc;

    public static GraphicalStatusEnum getByValue(Integer val) {
        return Arrays.stream(GraphicalStatusEnum.values()).filter(item -> item.value.equals(val)).findFirst().orElse(null);
    }
}

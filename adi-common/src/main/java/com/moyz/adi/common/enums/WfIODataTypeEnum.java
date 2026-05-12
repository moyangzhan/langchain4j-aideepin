package com.moyz.adi.common.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.Arrays;

@Getter
@AllArgsConstructor
public enum WfIODataTypeEnum implements BaseEnum {
    TEXT(1, "Text"),
    NUMBER(2, "Number"),
    OPTIONS(3, "Dropdown"),
    FILES(4, "File List"),
    BOOL(5, "Boolean"),
    REF_INPUT(6, "Reference Node Input"),
    REF_OUTPUT(7, "Reference Node Output");

    private final Integer value;
    private final String desc;

    public static WfIODataTypeEnum getByValue(Integer val) {
        return Arrays.stream(WfIODataTypeEnum.values()).filter(item -> item.value.equals(val)).findFirst().orElse(null);
    }
}

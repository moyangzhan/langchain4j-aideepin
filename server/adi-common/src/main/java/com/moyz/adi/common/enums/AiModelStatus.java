package com.moyz.adi.common.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum AiModelStatus implements BaseEnum {
    ACTIVE(1, "Active"),
    INACTIVE(2, "Inactive");

    private final Integer value;
    private final String desc;
}

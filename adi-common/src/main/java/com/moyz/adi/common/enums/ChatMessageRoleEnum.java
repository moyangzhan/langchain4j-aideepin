package com.moyz.adi.common.enums;


import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum ChatMessageRoleEnum implements BaseEnum {

    USER(1, "user"),
    SYSTEM(2, "system"),

    ASSISTANT(3, "assistant");

    private final Integer value;
    private final String desc;
}

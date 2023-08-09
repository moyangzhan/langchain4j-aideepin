package com.moyz.adi.common.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum UserStatusEnum implements BaseEnum {

    WAIT_CONFIRM(1, "待验证"),
    NORMAL(2, "正常"),
    FREEZE(3, "冻结");

    private final Integer value;
    private final String desc;

}

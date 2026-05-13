package com.moyz.adi.common.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.Arrays;

@Getter
@AllArgsConstructor
public enum UserStatusEnum implements BaseEnum {

    WAIT_CONFIRM(1, "Pending Verification"),
    NORMAL(2, "Active"),
    FREEZE(3, "Frozen");

    private final Integer value;
    private final String desc;

    public static UserStatusEnum getByValue(Integer val) {
        return Arrays.stream(UserStatusEnum.values()).filter(item -> item.value.equals(val)).findFirst().orElse(null);
    }

}

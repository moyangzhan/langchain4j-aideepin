package com.moyz.adi.common.workflow.node.switcher;

import lombok.Getter;

import java.util.Arrays;

@Getter
public enum OperatorEnum {
    CONTAINS("contains", "Contains"),

    NOT_CONTAINS("not contains", "Not Contains"),

    START_WITH("start with", "Starts With"),

    END_WITH("end with", "Ends With"),

    EMPTY("empty", "Is Empty"),

    NOT_EMPTY("not empty", "Is Not Empty"),

    EQUAL("=", "Equals"),

    NOT_EQUAL("!=", "Not Equals"),

    GREATER(">", "Greater Than"),

    GREATER_OR_EQUAL(">=", "Greater or Equal"),

    LESS("<", "Less Than"),

    LESS_OR_EQUAL("<=", "Less or Equal");

    private final String name;

    private final String desc;

    OperatorEnum(String name, String desc) {
        this.name = name;
        this.desc = desc;
    }

    public static OperatorEnum getByName(String name) {
        return Arrays.stream(OperatorEnum.values()).filter(item -> item.name.equals(name)).findFirst().orElse(null);
    }
}

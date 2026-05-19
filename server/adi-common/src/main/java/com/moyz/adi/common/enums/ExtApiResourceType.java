package com.moyz.adi.common.enums;

import lombok.Getter;

/**
 * External API resource types.
 */
@Getter
public enum ExtApiResourceType {

    CHARACTER("character"),
    KNOWLEDGE("knowledge"),
    WORKFLOW("workflow");

    private final String value;

    ExtApiResourceType(String value) {
        this.value = value;
    }

    public static ExtApiResourceType fromValue(String value) {
        for (ExtApiResourceType type : values()) {
            if (type.value.equals(value)) {
                return type;
            }
        }
        return null;
    }
}

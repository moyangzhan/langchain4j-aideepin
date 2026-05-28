package com.moyz.adi.common.enums;

import lombok.Getter;

/**
 * External API resource types.
 */
@Getter
public enum ExtApiResourceType {

    CHARACTER("character"),
    KNOWLEDGE("knowledge"),
    WORKFLOW("workflow"),
    DRAW("draw"),
    MCP("mcp");

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

    /**
     * 判断是否为用户级资源类型
     */
    public boolean isUserLevel() {
        return this == DRAW || this == MCP;
    }
}

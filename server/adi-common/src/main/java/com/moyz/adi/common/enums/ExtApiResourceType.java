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
     * 该类型的外部 API Key 是否绑定到用户（draw/mcp），而非单个资源（character/knowledge/workflow）
     * <p>
     * Whether the external API key of this type is bound to the user (draw/mcp) rather than to a
     * single resource (character/knowledge/workflow).
     */
    public boolean isUserLevelKey() {
        return this == DRAW || this == MCP;
    }
}

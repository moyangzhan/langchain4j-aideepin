package com.moyz.adi.common.memory.vo;

import com.moyz.adi.common.entity.User;
import lombok.Builder;
import lombok.Getter;

/**
 * Parameters for long-term memory add operation.
 * <p>
 * 长期记忆添加操作的参数。
 */
@Getter
@Builder
public class MemoryAddParam {

    /**
     * Character ID to associate memory with.
     * <p>
     * 关联记忆的角色 ID。
     */
    private final Long characterId;

    /**
     * LLM model platform name.
     * <p>
     * LLM 模型平台名称。
     */
    private final String modelPlatform;

    /**
     * LLM model name.
     * <p>
     * LLM 模型名称。
     */
    private final String modelName;

    /**
     * User's original message.
     * <p>
     * 用户的原始消息。
     */
    private final String userMessage;

    /**
     * Assistant's reply message.
     * <p>
     * 助手的回复消息。
     */
    private final String assistantMessage;

    /**
     * Current user (passed explicitly to avoid ThreadLocal issues in async context).
     * <p>
     * 当前用户（显式传入，避免异步线程中 ThreadLocal 不可用的问题）。
     */
    private final User user;

    /**
     * Whether the LLM model is free for the user.
     * <p>
     * LLM 模型对用户是否免费。
     */
    private final boolean isFreeToken;
}

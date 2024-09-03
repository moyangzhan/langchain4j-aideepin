package com.moyz.adi.common.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 调整LLM的输入时产生的消息
 */
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Data
public class InputAdaptorMsg {

    public static final int TOKEN_TOO_MUCH_NOT = 0;
    public static final int TOKEN_TOO_MUCH_QUESTION = 1;
    public static final int TOKEN_TOO_MUCH_MEMORY = 2;
    public static final int TOKEN_TOO_MUCH_RETRIEVE_DOCS = 3;
    public static final int TOKEN_TOO_MUCH_MESSAGE = 4;

    private int tokenTooMuch = TOKEN_TOO_MUCH_NOT;
    private int userQuestionTokenCount;
    private int memoryTokenCount;
    private int retrievedDocsTokenCount;
    private int messagesTokenCount;
}

package com.moyz.adi.common.util;

import com.moyz.adi.common.rag.TokenEstimatorFactory;
import com.moyz.adi.common.rag.TokenEstimatorThreadLocal;
import com.moyz.adi.common.vo.InputAdaptorMsg;
import dev.langchain4j.model.TokenCountEstimator;
import lombok.extern.slf4j.Slf4j;

/**
 * Validates whether the user input exceeds the token limit based on the model's maxInputTokens setting.
 */
@Slf4j
public class InputAdaptor {

    public static InputAdaptorMsg isQuestionValid(String userQuestion, int maxInputTokens) {
        return isQuestionValid(userQuestion, maxInputTokens, TokenEstimatorFactory.create(TokenEstimatorThreadLocal.getTokenEstimator()));
    }

    public static InputAdaptorMsg isQuestionValid(String userQuestion, int maxInputTokens, TokenCountEstimator tokenizer) {
        InputAdaptorMsg result = new InputAdaptorMsg();
        result.setTokenTooMuch(InputAdaptorMsg.TOKEN_TOO_MUCH_NOT);

        int questionLength = tokenizer.estimateTokenCountInText(userQuestion);
        result.setUserQuestionTokenCount(questionLength);
        if (questionLength > maxInputTokens) {
            log.warn("User question too long, exceeded {} tokens", maxInputTokens);
            result.setTokenTooMuch(InputAdaptorMsg.TOKEN_TOO_MUCH_QUESTION);
        }
        return result;
    }
}

package com.moyz.adi.common.util;

import com.moyz.adi.common.vo.LLMException;
import com.theokanning.openai.OpenAiError;
import dev.ai4j.openai4j.OpenAiHttpException;

public class OpenAiUtil {

    /**
     * openai错误格式：
     * dev.ai4j.openai4j.OpenAiHttpException: {
     *   "error": {
     *     "code": "content_policy_violation",
     *     "message": "Your request was rejected as a result of our safety system. Your prompt may contain text that is not allowed by our safety system.",
     *     "param": null,
     *     "type": "invalid_request_error"
     *   }
     * }
     * @param error 异常object
     * @return 通用的异常信息
     */
    public static LLMException parseError(Object error) {
        if (error instanceof OpenAiHttpException openAiHttpException) {
            OpenAiError openAiError = JsonUtil.fromJson(openAiHttpException.getMessage(), OpenAiError.class);
            if (null != openAiError) {
                OpenAiError.OpenAiErrorDetails errorDetails = openAiError.getError();
                LLMException llmException = new LLMException();
                llmException.setType(errorDetails.getType());
                llmException.setCode(errorDetails.getCode());
                llmException.setMessage(errorDetails.getMessage());
                return llmException;
            }
        }
        return null;
    }
}

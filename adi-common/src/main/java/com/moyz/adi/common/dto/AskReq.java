package com.moyz.adi.common.dto;

import com.moyz.adi.common.annotation.AskReqCheck;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.hibernate.validator.constraints.Length;

@Schema(description = "对话的请求对象")
@Data
@AskReqCheck
public class AskReq {

    @Length(min = 32, max = 32)
    private String conversationUuid;

    private String parentMessageId;

    private String prompt;

    /**
     * If not empty, it means will request AI with the exist prompt, param {@code prompt} is ignored
     */
    private String regenerateQuestionUuid;
}

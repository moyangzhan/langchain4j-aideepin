package com.moyz.adi.common.validator;

import com.moyz.adi.common.annotation.AskReqCheck;
import com.moyz.adi.common.dto.AskReq;
import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import org.apache.commons.lang3.StringUtils;

import java.util.regex.Pattern;

public class AskReqValidator implements
        ConstraintValidator<AskReqCheck, AskReq> {

    @Override
    public void initialize(AskReqCheck constraintAnnotation) {
        //无需校验
    }

    @Override
    public boolean isValid(AskReq value, ConstraintValidatorContext context) {
        if (StringUtils.isAllBlank(value.getPrompt(), value.getRegenerateQuestionUuid(), value.getAudioUuid())) {
            throw new IllegalArgumentException("prompt and regenerateMsgUuid are empty");
        }

        String uuidRegex = "^[0-9a-fA-F]{8}[0-9a-fA-F]{4}4[0-9a-fA-F]{3}[89abAB][0-9a-fA-F]{3}[0-9a-fA-F]{12}$";
        //check conversation uuid
        boolean isValid = Pattern.matches(uuidRegex, value.getConversationUuid());
        if (!isValid) {
            throw new IllegalArgumentException("conversation uuid error");
        }
        //check regenerate msg uuid
        if (StringUtils.isNotBlank(value.getRegenerateQuestionUuid())) {
            boolean isValid2 = Pattern.matches(uuidRegex, value.getRegenerateQuestionUuid());
            if (!isValid2) {
                throw new IllegalArgumentException("msg uuid error");
            }
        }
        return true;
    }
}

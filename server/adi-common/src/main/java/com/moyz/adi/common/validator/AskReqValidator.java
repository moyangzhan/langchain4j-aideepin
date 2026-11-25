package com.moyz.adi.common.validator;

import com.moyz.adi.common.annotation.AskReqCheck;
import com.moyz.adi.common.dto.AskReq;
import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import org.apache.commons.lang3.StringUtils;

import java.util.regex.Pattern;

public class AskReqValidator implements
        ConstraintValidator<AskReqCheck, AskReq> {

    private static final String UUID_REGEX = "^[0-9a-fA-F]{32}$";

    @Override
    public void initialize(AskReqCheck constraintAnnotation) {
        //无需校验
    }

    @Override
    public boolean isValid(AskReq value, ConstraintValidatorContext context) {
        if (StringUtils.isAllBlank(value.getPrompt(), value.getRegenerateQuestionUuid(), value.getAudioUuid())) {
            return reject(context, "prompt, regenerateQuestionUuid and audioUuid are all empty");
        }
        if (!isUuid(value.getCharacterUuid())) {
            return reject(context, "characterUuid is invalid");
        }
        if (StringUtils.isNotBlank(value.getRegenerateQuestionUuid()) && !isUuid(value.getRegenerateQuestionUuid())) {
            return reject(context, "regenerateQuestionUuid is invalid");
        }
        return true;
    }

    private boolean reject(ConstraintValidatorContext context, String message) {
        context.disableDefaultConstraintViolation();
        context.buildConstraintViolationWithTemplate(message).addConstraintViolation();
        return false;
    }

    private boolean isUuid(String value) {
        return StringUtils.isNotBlank(value) && Pattern.matches(UUID_REGEX, value);
    }
}

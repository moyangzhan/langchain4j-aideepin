package com.moyz.adi.common.validator;

import com.moyz.adi.common.annotation.CreateImageReqCheck;
import com.moyz.adi.common.dto.CreateImageDto;
import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import org.apache.commons.lang3.StringUtils;

import static com.moyz.adi.common.cosntant.AdiConstant.GenerateImage.*;

public class CreateImageReqValidator implements
        ConstraintValidator<CreateImageReqCheck, CreateImageDto> {
    @Override
    public boolean isValid(CreateImageDto createImageDto, ConstraintValidatorContext constraintValidatorContext) {
        if (createImageDto.getInteractingMethod() == INTERACTING_METHOD_GENERATE_IMAGE && StringUtils.isBlank(createImageDto.getPrompt())) {
            throw new IllegalArgumentException("Prompt can not be empty");
        }
        return true;
    }
}

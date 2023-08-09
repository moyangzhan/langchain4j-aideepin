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
        int interactingMethod = createImageDto.getInteractingMethod();
        if (interactingMethod == INTERACTING_METHOD_GENERATE_IMAGE && StringUtils.isBlank(createImageDto.getPrompt())) {
            throw new IllegalArgumentException("Prompt can not be empty");
        } else if (interactingMethod == INTERACTING_METHOD_EDIT_IMAGE
                && StringUtils.isAnyBlank(createImageDto.getOriginalImage(), createImageDto.getMaskImage(), createImageDto.getPrompt())) {
            throw new IllegalArgumentException("Edit image,mask image or prompt can not be empty");
        } else if (interactingMethod == INTERACTING_METHOD_VARIATION && StringUtils.isBlank(createImageDto.getOriginalImage())) {
            throw new IllegalArgumentException("Edit image can not be empty");
        }
        return true;
    }
}

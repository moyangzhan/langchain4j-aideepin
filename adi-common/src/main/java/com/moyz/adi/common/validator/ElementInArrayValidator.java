package com.moyz.adi.common.validator;

import com.moyz.adi.common.annotation.ElementInArray;
import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;

import java.util.Arrays;

public class ElementInArrayValidator implements ConstraintValidator<ElementInArray, String> {

    private String[] acceptedValues;
    private boolean required;

    @Override
    public void initialize(ElementInArray constraintAnnotation) {
        this.acceptedValues = constraintAnnotation.acceptedValues();
        this.required = constraintAnnotation.required();
    }

    @Override
    public boolean isValid(String value, ConstraintValidatorContext context) {
        if (value == null && this.required) {
            return false;
        }
        return Arrays.asList(acceptedValues).contains(value);
    }
}

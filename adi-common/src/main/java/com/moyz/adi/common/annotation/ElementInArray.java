package com.moyz.adi.common.annotation;

import com.moyz.adi.common.validator.ElementInArrayValidator;
import jakarta.validation.Constraint;
import jakarta.validation.Payload;

import java.lang.annotation.*;

@Documented
@Constraint(validatedBy = ElementInArrayValidator.class)
@Target({ElementType.FIELD})
@Retention(RetentionPolicy.RUNTIME)
public @interface ElementInArray {
    String message() default "Element not found in the array";

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};

    String[] acceptedValues();

    boolean required() default false;
}
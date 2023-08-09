package com.moyz.adi.common.annotation;

import com.moyz.adi.common.validator.CreateImageReqValidator;
import jakarta.validation.Constraint;
import jakarta.validation.Payload;

import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.*;
import static java.lang.annotation.RetentionPolicy.RUNTIME;


@Constraint(validatedBy = {
        CreateImageReqValidator.class,
})
@Target({TYPE, FIELD, PARAMETER, METHOD, CONSTRUCTOR, ANNOTATION_TYPE})
@Retention(RUNTIME)
@Documented
public @interface CreateImageReqCheck {
    String message() default "dddd";

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};
}

package com.moyz.adi.common.annotation;

import com.moyz.adi.common.validator.AskReqValidator;
import jakarta.validation.Constraint;
import jakarta.validation.Payload;

import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.*;
import static java.lang.annotation.ElementType.ANNOTATION_TYPE;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

@Constraint(validatedBy = {
        AskReqValidator.class,
})
@Target({TYPE, FIELD, PARAMETER, METHOD, CONSTRUCTOR, ANNOTATION_TYPE})
@Retention(RUNTIME)
@Documented
public @interface NotAllFieldsEmptyCheck {
    String message() default "all filed is null";

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};
}

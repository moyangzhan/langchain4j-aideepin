package com.moyz.adi.common.validator;

import com.moyz.adi.common.annotation.NotAllFieldsEmptyCheck;
import com.moyz.adi.common.dto.UserUpdateReq;
import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import org.apache.commons.lang3.StringUtils;

import java.lang.reflect.Field;

public class NotAllFieldsNullValidator implements
        ConstraintValidator<NotAllFieldsEmptyCheck, UserUpdateReq> {

    @Override
    public void initialize(NotAllFieldsEmptyCheck constraintAnnotation) {
    }

    @Override
    public boolean isValid(UserUpdateReq value, ConstraintValidatorContext context) {
        Field[] fields = UserUpdateReq.class.getDeclaredFields();
        try {
            for (Field field : fields) {
                Object object = field.get(value);
                if (object instanceof String) {
                    return StringUtils.isNotBlank((String) object);
                } else if (null != object) {
                    return true;
                }
            }
        } catch (IllegalAccessException e) {
            throw new RuntimeException(e);
        }
        return true;
    }
}

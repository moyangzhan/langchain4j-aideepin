package com.moyz.adi.common.exception;


import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.util.SpringUtil;

import java.text.MessageFormat;
import java.util.Locale;

public class BaseException extends RuntimeException {
    private final String code;
    private final String info;

    private Object data;

    public BaseException(String code, String info) {
        super(code + ":" + info);
        this.code = code;
        this.info = info;
    }

    public BaseException(ErrorEnum errorEnum, String... infoValues) {
        super(errorEnum.getCode() + ":" + resolveMessage(errorEnum.getInfo(), infoValues));
        this.code = errorEnum.getCode();
        this.info = resolveMessage(errorEnum.getInfo(), infoValues);
    }

    public String getCode() {
        return code;
    }

    public String getInfo() {
        return info;
    }

    public Object getData() {
        if (null != data) {
            return data;
        }
        return getMessage();
    }

    public BaseException setData(Object data) {
        this.data = data;
        return this;
    }

    private static String resolveMessage(String key, String... infoValues) {
        String message = SpringUtil.getMessage(key);
        if (infoValues.length > 0) {
            return MessageFormat.format(message, (Object[]) infoValues);
        }
        return message;
    }
}

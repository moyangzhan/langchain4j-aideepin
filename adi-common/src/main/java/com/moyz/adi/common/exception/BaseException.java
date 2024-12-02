package com.moyz.adi.common.exception;


import com.moyz.adi.common.enums.ErrorEnum;

import java.text.MessageFormat;

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
        super(errorEnum.getCode() + ":" + MessageFormat.format(errorEnum.getInfo(), infoValues));
        this.code = errorEnum.getCode();
        if (infoValues.length > 0) {
            this.info = MessageFormat.format(errorEnum.getInfo(), infoValues);
        } else {
            this.info = errorEnum.getInfo();
        }
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
}

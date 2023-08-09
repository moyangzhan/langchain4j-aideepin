package com.moyz.adi.common.config;

import com.moyz.adi.common.base.BaseResponse;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import java.util.HashMap;
import java.util.Map;

@Slf4j
@RestControllerAdvice
public class GlobalExceptionHandler {
    /**
     * 参数校验异常
     *
     * @return BaseResponse
     */
    @ExceptionHandler(MethodArgumentNotValidException.class)
    private BaseResponse handleMethodArgumentNotValidException(
            final MethodArgumentNotValidException exception) {
        Map<Object, Object> error = wrapperError(exception.getBindingResult());
        log.error("参数校验异常:{}", error);
        return new BaseResponse(ErrorEnum.A_PARAMS_ERROR.getCode(), ErrorEnum.A_PARAMS_ERROR.getInfo(), error);
    }

    @ExceptionHandler(BaseException.class)
    private BaseResponse handleBaseException(final BaseException exception) {
        log.error("拦截业务异常:{}", exception);
        return new BaseResponse(exception.getCode(), exception.getInfo(), exception.getData());
    }

    /**
     * 兜底
     *
     * @return BaseResponse
     */
    @ExceptionHandler(Exception.class)
    private BaseResponse handleException(final Exception exception) {
        log.error("拦截全局异常:", exception);
        return new BaseResponse(ErrorEnum.B_GLOBAL_ERROR.getCode(), ErrorEnum.B_GLOBAL_ERROR.getInfo(), exception.getMessage());
    }

    private Map<Object, Object> wrapperError(BindingResult result) {
        Map<Object, Object> errorMap = new HashMap<>(5);
        result.getFieldErrors().forEach(x -> errorMap.put(x.getField(), x.getDefaultMessage()));
        return errorMap;
    }
}

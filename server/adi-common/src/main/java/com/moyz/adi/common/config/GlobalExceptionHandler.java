package com.moyz.adi.common.config;

import com.moyz.adi.common.base.BaseResponse;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.util.SpringUtil;
import jakarta.annotation.Resource;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.MediaType;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import tools.jackson.databind.ObjectMapper;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;

@Slf4j
@RestControllerAdvice
public class GlobalExceptionHandler {

    @Resource
    private ObjectMapper objectMapper;

    /**
     * 参数校验异常
     */
    @ExceptionHandler(MethodArgumentNotValidException.class)
    private void handleMethodArgumentNotValidException(
            final MethodArgumentNotValidException exception, HttpServletResponse response) throws IOException {
        Map<Object, Object> error = wrapperError(exception.getBindingResult());
        log.error("Parameter validation error:{}", error);
        writeJson(response, new BaseResponse(ErrorEnum.A_PARAMS_ERROR.getCode(), SpringUtil.getMessage(ErrorEnum.A_PARAMS_ERROR.getInfo()), error));
    }

    @ExceptionHandler(BaseException.class)
    private void handleBaseException(final BaseException exception, HttpServletResponse response) throws IOException {
        log.error("Business exception intercepted:{}", exception);
        writeJson(response, new BaseResponse(exception.getCode(), exception.getInfo(), exception.getData()));
    }

    /**
     * 兜底
     */
    @ExceptionHandler(Exception.class)
    private void handleException(final Exception exception, HttpServletResponse response) throws IOException {
        log.error("Global exception intercepted:", exception);
        writeJson(response, new BaseResponse(ErrorEnum.B_GLOBAL_ERROR.getCode(), SpringUtil.getMessage(ErrorEnum.B_GLOBAL_ERROR.getInfo()), null));
    }

    /**
     * Write JSON directly to the response without content negotiation,
     * so SSE endpoints (Accept: text/event-stream) can still receive a JSON error body.
     */
    private void writeJson(HttpServletResponse response, BaseResponse<?> resp) throws IOException {
        response.setContentType(MediaType.APPLICATION_JSON_VALUE);
        response.setCharacterEncoding(StandardCharsets.UTF_8.name());
        response.getWriter().write(objectMapper.writeValueAsString(resp));
    }

    private Map<Object, Object> wrapperError(BindingResult result) {
        Map<Object, Object> errorMap = new HashMap<>(5);
        result.getFieldErrors().forEach(x -> errorMap.put(x.getField(), x.getDefaultMessage()));
        result.getGlobalErrors().forEach(x -> errorMap.put(x.getObjectName(), x.getDefaultMessage()));
        return errorMap;
    }
}

package com.moyz.adi.common.base;

import com.moyz.adi.common.util.JsonUtil;
import lombok.extern.slf4j.Slf4j;
import org.jetbrains.annotations.NotNull;
import org.springframework.core.MethodParameter;
import org.springframework.http.MediaType;
import org.springframework.http.server.ServerHttpRequest;
import org.springframework.http.server.ServerHttpResponse;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.servlet.mvc.method.annotation.ResponseBodyAdvice;

@Slf4j
@RestControllerAdvice(basePackages = {"com.moyz.adi"})
public class ResponseWrapper implements ResponseBodyAdvice<Object> {

    @Override
    public boolean supports(MethodParameter methodParameter, Class aClass) {
        return true;
    }

    @NotNull
    @Override
    public Object beforeBodyWrite(Object result, MethodParameter methodParameter,
                                  MediaType mediaType, Class aClass, ServerHttpRequest serverHttpRequest,
                                  ServerHttpResponse serverHttpResponse) {
        if (result instanceof BaseResponse) {
            return result;
        } else if (result instanceof String str) {
            return JsonUtil.toJson(new BaseResponse<>(true, str));
        } else if (result instanceof org.springframework.core.io.Resource) {
            return result;
        }
        return new BaseResponse<>(true, result);
    }

}
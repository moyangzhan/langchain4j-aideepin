package com.moyz.adi.common.config;

import com.moyz.adi.common.helper.HttpHelper;
import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.annotation.Resource;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpMethod;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.servlet.HandlerInterceptor;
import org.springframework.web.servlet.ModelAndView;

@Slf4j
//@Service
public class LogInterceptor implements HandlerInterceptor {

    @Resource
    private ObjectMapper objectMapper;

    @Override
    public boolean preHandle(HttpServletRequest request, HttpServletResponse response,
                             Object handler) throws Exception {
        if (handler instanceof HandlerMethod) {
            HandlerMethod method = (HandlerMethod) handler;
            if (HttpMethod.GET.matches(request.getMethod())) {
                log.info("url:{},ip:{},method:{},param:{}", request.getRequestURL(),
                        request.getRemoteAddr(), method.getMethod().getName(),
                        objectMapper.writeValueAsString(request.getParameterMap()));
            } else {
                String bodyString = HttpHelper.getBodyString(request);
                log.info("url:{},ip:{},method：{},param:{},body:{}", request.getRequestURL(),
                        request.getRemoteAddr(), method.getMethod().getName(),
                        objectMapper.writeValueAsString(request.getParameterMap()), bodyString);
            }
        } else {
            log.info("url:{},ip:{}", request.getRequestURL(),
                    request.getRemoteAddr());
        }

        return true;
    }

    @Override
    public void postHandle(HttpServletRequest request, HttpServletResponse response, Object handler,
                           ModelAndView modelAndView) throws Exception {

    }

    @Override
    public void afterCompletion(HttpServletRequest request, HttpServletResponse response,
                                Object handler, Exception ex) throws Exception {

    }
}

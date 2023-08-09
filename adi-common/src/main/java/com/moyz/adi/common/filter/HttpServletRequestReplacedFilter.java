package com.moyz.adi.common.filter;

import com.moyz.adi.common.config.RequestReaderHttpServletRequestWrapper;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;

/**
 * 过滤器
 * 当前主要是配合LogInterceptor使用，已使用ControllerParamsLogAspect代替LogInterceptor
 */
@Slf4j
//@Component
@Order(Ordered.HIGHEST_PRECEDENCE)
public class HttpServletRequestReplacedFilter extends OncePerRequestFilter {

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain)
            throws ServletException, IOException {
        log.info("HttpServletRequestReplacedFilter:" + request.getRequestURI());
        ServletRequest requestWrapper = new RequestReaderHttpServletRequestWrapper(request);
        filterChain.doFilter(requestWrapper, response);
    }

}
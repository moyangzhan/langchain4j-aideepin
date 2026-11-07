package com.moyz.adi.common.interceptor;

import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.entity.User;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.servlet.HandlerInterceptor;

/**
 * 管理端角色闸：按处理器所在包判定，与 URL 前缀、上下文路径、URL 规范化无关；
 * 管理端授权的唯一闸门，认证由 TokenFilter 负责。
 * Admin role gate resolved from the handler's package, independent of URL prefixes, the
 * context path and URL normalization; the single authorization gate for the admin area,
 * with authentication handled by TokenFilter.
 */
@Slf4j
@Component
public class AdminAuthInterceptor implements HandlerInterceptor {

    private static final String ADMIN_PACKAGE_PREFIX = "com.moyz.adi.admin";

    @Override
    public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler) {
        if (handler instanceof HandlerMethod handlerMethod
                && handlerMethod.getBeanType().getPackageName().startsWith(ADMIN_PACKAGE_PREFIX)) {
            User user = ThreadContext.getCurrentUser();
            if (null == user || !Boolean.TRUE.equals(user.getIsAdmin())) {
                log.warn("No admin permission:{}", request.getRequestURI());
                response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
                return false;
            }
        }
        return true;
    }
}

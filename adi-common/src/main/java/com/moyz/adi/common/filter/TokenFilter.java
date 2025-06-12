package com.moyz.adi.common.filter;

import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.cosntant.RedisKeyConstant;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.util.JsonUtil;
import io.micrometer.common.util.StringUtils;
import jakarta.annotation.Resource;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;
import java.text.MessageFormat;

import static org.springframework.http.HttpHeaders.AUTHORIZATION;

@Slf4j
@Component
public class TokenFilter extends OncePerRequestFilter {

    protected static final String[] EXCLUDE_API = {
            "/auth/",
            "/model/",
            "/user/avatar/",
            "/draw/public/",
            "/draw/detail/",
            "/draw/comment/",
            "/knowledge-base/public/",
            "/workflow/public",
            "/mcp/public"
    };

    protected static final String[] TOKEN_IN_PARAMS = {
            "/my-image/",
            "/my-thumbnail/",
            "/image/",
            "/file/"
    };

    @Resource
    private StringRedisTemplate stringRedisTemplate;

    @Value("${server.servlet.context-path:}")
    private String contextPath;

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain) throws ServletException, IOException {
        String requestUri = request.getRequestURI();
        String token = request.getHeader(AUTHORIZATION);
        if (StringUtils.isBlank(token) && checkPathWithToken(requestUri)) {
            token = request.getParameter("token");
        }
        if (excludePath(requestUri)) {

            if (StringUtils.isNotBlank(token)) {
                String tokenKey = MessageFormat.format(RedisKeyConstant.USER_TOKEN, token);
                String userJson = stringRedisTemplate.opsForValue().get(tokenKey);
                if (StringUtils.isNotBlank(userJson)) {
                    User user = JsonUtil.fromJson(userJson, User.class);
                    if (null != user) {
                        ThreadContext.setCurrentUser(user);
                        ThreadContext.setToken(token);
                    }
                }
            }
            filterChain.doFilter(request, response);
        } else if (StringUtils.isNotBlank(token)) {
            String tokenKey = MessageFormat.format(RedisKeyConstant.USER_TOKEN, token);
            String userJson = stringRedisTemplate.opsForValue().get(tokenKey);
            if (StringUtils.isBlank(userJson)) {
                log.warn("未登录:{}", requestUri);
                response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
                return;
            }

            User user = JsonUtil.fromJson(userJson, User.class);
            if (null == user) {
                log.warn("用户不存在:{}", requestUri);
                response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
                return;
            }
            if (Boolean.TRUE.equals(!user.getIsAdmin()) && requestUri.startsWith("/admin/")) {
                log.warn("无管理权限:{}", requestUri);
                response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
                return;
            }
            ThreadContext.setCurrentUser(user);
            ThreadContext.setToken(token);
            filterChain.doFilter(request, response);
        } else {
            log.warn("未授权:{}", requestUri);
            response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
        }
    }

    private boolean excludePath(String requestUri) {
        for (String path : EXCLUDE_API) {
            if (requestUri.startsWith(contextPath + path)) {
                return true;
            }
        }
        for (String path : AdiConstant.WEB_RESOURCES) {
            if (requestUri.startsWith(contextPath + path) || requestUri.endsWith(path)) {
                return true;
            }
        }
        return false;
    }

    private boolean checkPathWithToken(String requestUri) {
        for (String path : TOKEN_IN_PARAMS) {
            if (requestUri.startsWith(contextPath + path)) {
                return true;
            }
        }
        return false;
    }
}

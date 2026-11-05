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
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;
import java.text.MessageFormat;

import static org.springframework.http.HttpHeaders.AUTHORIZATION;

/**
 * 认证层：token 解析与 ThreadContext 装配；admin 授权在 AdminAuthInterceptor。
 * 外部 API 请求只凭 {@link ExtApiAuthFilter} 设置的上下文标记放行，不按路径放行。
 * <p>
 * Authentication only: resolves tokens and populates ThreadContext; admin authorization
 * lives in AdminAuthInterceptor. External API requests pass only on the context flag
 * set by {@link ExtApiAuthFilter}, never by path.
 * </p>
 */
@Slf4j
@Component
@Order(Ordered.LOWEST_PRECEDENCE - 1)
public class TokenFilter extends OncePerRequestFilter {

    protected static final String[] EXCLUDE_API = {
            "/auth/",
            "/model/",
            "/user/avatar/",
            "/draw/public/",
            "/draw/detail/",
            "/draw/comment/list/",
            "/knowledge-base/public/",
            "/workflow/public/",
            "/mcp/public/",
            "/sys/config/public/",
    };

    protected static final String[] TOKEN_IN_PARAMS = {
            "/my-image/",
            "/my-thumbnail/",
            "/image/",
            "/file/"
    };

    @Resource
    private StringRedisTemplate stringRedisTemplate;

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain) throws ServletException, IOException {
        try {
            if (ThreadContext.isExtApiRequest()) {
                filterChain.doFilter(request, response);
                return;
            }
            String requestUri = request.getRequestURI();
            String path = requestUri.substring(request.getContextPath().length());
            String token = request.getHeader(AUTHORIZATION);
            if (StringUtils.isBlank(token) && checkPathWithToken(path)) {
                token = request.getParameter("token");
            }
            if (excludePath(path)) {
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
                    log.warn("Not logged in:{}", requestUri);
                    response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
                    return;
                }

                User user = JsonUtil.fromJson(userJson, User.class);
                if (null == user) {
                    log.warn("User not found:{}", requestUri);
                    response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
                    return;
                }
                ThreadContext.setCurrentUser(user);
                ThreadContext.setToken(token);
                filterChain.doFilter(request, response);
            } else {
                log.warn("Unauthorized:{}", requestUri);
                response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
            }
        } finally {
            // External API requests are cleaned up by ExtApiAuthFilter
            if (!ThreadContext.isExtApiRequest()) {
                ThreadContext.unload();
            }
        }
    }

    /**
     * 尾斜杠条目按子树前缀匹配，无尾斜杠条目按精确端点匹配。
     * Entries with a trailing slash match the subtree below them; entries without it match the exact endpoint.
     */
    private static boolean matchesEntry(String path, String entry) {
        return entry.endsWith("/") ? path.startsWith(entry) : path.equals(entry);
    }

    private boolean excludePath(String path) {
        for (String excluded : EXCLUDE_API) {
            if (matchesEntry(path, excluded)) {
                return true;
            }
        }
        for (String resource : AdiConstant.WEB_RESOURCES) {
            if (path.startsWith(resource) || path.endsWith(resource)) {
                return true;
            }
        }
        return false;
    }

    private boolean checkPathWithToken(String path) {
        for (String tokenInParams : TOKEN_IN_PARAMS) {
            if (matchesEntry(path, tokenInParams)) {
                return true;
            }
        }
        return false;
    }
}
